{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- TODO Stop using String. Use Text!

module Main where

import           Shelly hiding (FilePath,path,(</>),(<.>))

import           Data.String

import           Debug.Trace
import qualified Distribution.Verbosity                        as Verbosity
import           Test.QuickCheck

import qualified Data.Set as Set
import           Data.Set (Set)

import qualified Data.Text as Text
import           Data.Text (Text)

--port           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Monoid
import           Data.Aeson                                    as JSON
import qualified Data.ByteString.Lazy                          as LBS
import qualified Data.ByteString.Lazy.Char8                    as BC
import           Data.List                                     (intersperse)
import           Data.List.Split
import           Data.Maybe

import           Distribution.Package                          as Cabal
import           Distribution.PackageDescription               as Cabal
import           Distribution.PackageDescription.Configuration as Cabal
import           Distribution.PackageDescription.Parse         as Cabal

import qualified Documentation.Haddock                         as Haddock

import qualified System.Directory                              as Sys
import qualified System.Environment                            as Sys
import           System.FilePath.Find                          ((&&?), (==?))
import qualified System.FilePath.Find                          as P
import           System.IO
import           System.IO.Error
import qualified System.Path                                   as P
import           System.Path ((</>),(<.>))

import           GHC
import           Name


-- Dependencies --------------------------------------------------------------

data RawDependency = RawDependency String deriving (Show,Eq,Ord)
data ResolvedDependency = ResolvedDependency String deriving (Show,Eq,Ord)

rawDependency ∷ ResolvedDependency → RawDependency
rawDependency (ResolvedDependency d) = RawDependency d

instance ToJSON RawDependency where
  toJSON (RawDependency s) = toJSON s

instance ToJSON ResolvedDependency where
  toJSON dep@(ResolvedDependency nm) =
    object [ "Raw" .= rawDependency dep
           , "Target" .= object [ "ToRepoCloneURL" .= (""∷String)
                                , "ToUnit" .= nm
                                , "ToUnitType" .= ("HaskellPackage"∷String)
                                , "ToVersionString" .= (""∷String)
                                , "ToRevSpec" .= (""∷String)
                                ]
           ]

instance (Ord a, Arbitrary a) => Arbitrary(Set a) where
  arbitrary = Set.fromList <$> arbitrary

instance Arbitrary RawDependency where
  arbitrary = RawDependency <$> arbitrary



-- Source Units --------------------------------------------------------------

-- All paths are relative to repository root.
data CabalInfo = CabalInfo
   { cabalFile         ∷ P.RelFile
   , cabalPkgName      ∷ String
   , cabalDependencies ∷ Set RawDependency
   , cabalSrcFiles     ∷ Set P.RelFile
   , cabalSrcDirs      ∷ Set P.RelDir
   } deriving (Show,Eq)

instance FromJSON CabalInfo where
 parseJSON (Object v) = do
    infoObj ← v .: "Data"
    case infoObj of
      Object info → do
        path ← P.asRelFile <$> info .: "Path"
        name ← v .: "Name"
        deps ← Set.map RawDependency <$> v .: "Dependencies"
        files ← Set.map P.asRelFile <$> v .: "Files"
        dirs ← Set.map P.asRelDir <$> info .: "Dirs"
        return $ CabalInfo path name deps files dirs
      _ → mzero
 parseJSON _ = mzero

instance ToJSON CabalInfo where
  toJSON (CabalInfo path name deps files dirs) =
    let dir = P.dropFileName path in
      object [ "Type" .= ("HaskellPackage"∷String)
             , "Ops" .= object ["graph" .= Null, "depresolve" .= Null]
             , "Name" .= name
             , "Dir" .= P.getPathString dir
             , "Globs" .= Set.map (P.getPathString >>> (<> "/**/*.hs")) dirs
             , "Files" .= Set.map P.getPathString files
             , "Dependencies" .= deps
             , "Data" .= object [ "Path" .= P.getPathString path
                                , "Dirs" .= Set.map P.getPathString dirs
                                ]
             , "Repo" .= Null
             , "Config" .= Null
             ]

-- TODO pathtype's Gen instances output far too much data. Hack around it.
newtype PathHack a b = PathHack (P.Path a b)
instance Arbitrary (PathHack a b) where
  arbitrary = return $ PathHack $ P.asPath "./asdf"

unPathHack ∷ PathHack a b → P.Path a b
unPathHack (PathHack x) = x

instance Arbitrary CabalInfo where
  arbitrary = do
    file ← unPathHack <$> arbitrary
    files ← Set.fromList <$> map unPathHack <$> arbitrary
    dirs ← Set.fromList <$> map unPathHack <$> arbitrary
    deps ← arbitrary
    name ← arbitrary
    return $ CabalInfo file name deps files dirs

prop_cabalInfoJson ∷ CabalInfo → Bool
prop_cabalInfoJson c = (Just c==) $ JSON.decode $ JSON.encode c


-- Source Graph --------------------------------------------------------------

-- TODO Making these both unsigned and making the second number a size would
--      make invalid ranges unrepresentables.

-- Loc is a filename with a span formed by two byte offsets.
type Loc = (FilePath,Integer,Integer)

type ModulePath = ([String],String)

data DefKind = Module | Value | Type
  deriving Show

data Def = Def { defModule ∷ ModulePath
               , defName   ∷ String
               , defKind   ∷ DefKind
               , defLoc    ∷ Loc
               }
  deriving Show

newtype Ref = Ref Def

modulePathString ∷ ModulePath → String
modulePathString (path,leaf) = joinL "." $ path ++ [leaf]

modulePathToSrclibPath ∷ ModulePath → String
modulePathToSrclibPath (path,leaf) = joinL "/" $ path ++ [leaf]

data Graph = Graph [Def]

joinL ∷ ∀a. [a] → [[a]] → [a]
joinL sep = concat <<< intersperse sep

instance ToJSON Def where
  toJSON d = object [ "Path" .= (modulePathToSrclibPath $ defModule d)
                    , "TreePath" .= (modulePathToSrclibPath $ defModule d)
                    , "Name" .= defName d
                    , "Kind" .= show (defKind d)
                    , "File" .= (case defLoc d of (fn,_,_)→fn)
                    , "DefStart" .= (case defLoc d of (_,s,_)→s)
                    , "DefEnd" .= (case defLoc d of (_,_,e)→e)
                    , "Exported" .= True
                    , "Test" .= False
                    , "JsonText" .= object[]
                    ]

instance ToJSON Ref where
  toJSON (Ref d) = object [ "DefRepo" .= ""
                          , "DefUnitType" .= ""
                          , "DefUnit" .= ""
                          , "DefPath" .= (modulePathToSrclibPath $ defModule d)
                          , "Def" .= True
                          , "File" .= (case defLoc d of (fn,_,_)→fn)
                          , "Start" .= (case defLoc d of (_,s,_)→s)
                          , "End" .= (case defLoc d of (_,_,e)→e)
                          ]

instance ToJSON Graph where
  toJSON (Graph defs) = object ["Docs".=e, "Refs".=(Ref<$>defs), "Defs".=defs]
    where e = []∷[String]

-- Scaning Repos and Parsing Cabal files -------------------------------------

findFiles ∷ P.FindClause Bool → P.AbsDir → IO [P.AbsFile]
findFiles q root = do
  let cond = P.fileType ==? P.RegularFile &&? q
  fileNames ← P.find P.always cond $ P.getPathString root
  return $ map P.asAbsPath fileNames

allDeps ∷ PackageDescription → Set RawDependency
allDeps desc = Set.fromList $ map toRawDep deps
  where deps = buildDepends desc ++ concatMap getDeps (allBuildInfo desc)
        toRawDep (Cabal.Dependency (PackageName nm) _) = RawDependency nm
        getDeps build = concat [ buildTools build
                               , pkgconfigDepends build
                               , targetBuildDepends build
                               ]

sourceDirs ∷ PackageDescription → [P.RelDir]
sourceDirs desc = map P.asRelDir $ librarySourceFiles ++ executableSourceFiles
  where librarySourceFiles =
          concat $ maybeToList $ (libBuildInfo>>>hsSourceDirs) <$> library desc
        executableSourceFiles =
          concat $ (buildInfo>>>hsSourceDirs) <$> executables desc

readCabalFile ∷ P.AbsDir → P.AbsFile → IO CabalInfo
readCabalFile repoDir cabalFilePath = do
  genPkgDesc ← readPackageDescription Verbosity.deafening $ show cabalFilePath
  let desc = flattenPackageDescription genPkgDesc
      PackageName name = pkgName $ package desc
      dirs = map (P.combine $ P.takeDirectory cabalFilePath) $ sourceDirs desc

  sourceFiles ← concat <$> mapM (findFiles$P.extension==?".hs") dirs
  return CabalInfo { cabalFile = P.makeRelative repoDir cabalFilePath
                   , cabalPkgName = name
                   , cabalDependencies = allDeps desc
                   , cabalSrcFiles = Set.fromList $ map (P.makeRelative repoDir) sourceFiles
                   , cabalSrcDirs = Set.fromList $ map (P.makeRelative repoDir) dirs
                   }


-- Resolve Dependencies ------------------------------------------------------

resolve ∷ RawDependency → IO ResolvedDependency
resolve (RawDependency d) = return (ResolvedDependency d)


-- Graph Symbol References ---------------------------------------------------

modulePathFromName ∷ String → ModulePath
modulePathFromName nm =
  case reverse $ splitOn "." nm of
    [] → error "Empty string is not a valid module name!"
    leaf:reversedPath → (reverse reversedPath, leaf)

moduleFileName ∷ ModulePath → P.RelFile
moduleFileName (path,nm) = relDirPath </> filename
  where relDirPath∷P.RelDir = foldl (</>) P.currentDir $ P.asRelPath<$>path
        filename∷P.RelFile = P.asRelFile nm <.> "hs"

findModuleFile ∷ [P.RelDir] → ModulePath → IO (Maybe P.RelFile)
findModuleFile rootDirs modul = do
  viablePaths ← flip mapM rootDirs $ \r → do
    let fn∷P.RelFile = r </> moduleFileName modul
    exists ← Sys.doesFileExist $ show fn
    return $ if exists then Just fn else Nothing
  return $ listToMaybe $ catMaybes viablePaths

-- TODO Stop silently ignoring errors. Specifically, EOF errors and invalid
--      lines and columns. Also, handle the case where the file doesn't exist.
getLocOffset ∷ FilePath → (Int,Int) → IO Integer
getLocOffset path (line,col) =
  flip catchIOError (\e → if isEOFError e then return 0 else ioError e) $
    withFile path ReadMode $ \h → do
      let (lineOffset, colOffset) = (max (line-1) 0, max (col-1) 0)
      replicateM_ lineOffset $ hGetLine h
      replicateM_ colOffset $ hGetChar h
      hTell h

srcSpanLoc ∷ FilePath → SrcSpan → IO (Maybe Loc)
srcSpanLoc _ (UnhelpfulSpan _) = return Nothing
srcSpanLoc fn (RealSrcSpan r) = do
  let l1 = srcSpanStartLine r
      c1 = srcSpanStartCol r
      l2 = srcSpanEndLine r
      c2 = srcSpanEndCol r
  startOffset ← getLocOffset fn (l1,c1)
  endOffset ← getLocOffset fn (l2,c2)
  return $ Just (fn,startOffset,endOffset)


-- Toolchain Command-Line Interface ------------------------------------------

scanCmd ∷ IO [CabalInfo]
scanCmd = do
  cwd ← Sys.getCurrentDirectory
  let root = P.asAbsDir cwd
  cabalFiles ← findFiles (P.extension ==? ".cabal") root
  mapM (readCabalFile root) cabalFiles

-- TODO This is not exception safe! Use a bracket?
withWorkingDirectory ∷ P.AbsRelClass ar ⇒ P.DirPath ar → IO a → IO a
withWorkingDirectory dir action = do
  oldDir ← Sys.getCurrentDirectory
  Sys.setCurrentDirectory(P.getPathString dir)
  result ← action
  Sys.setCurrentDirectory oldDir
  return result

-- TODO Haddock seems to strip location information from ‘Name’s, we
-- should be able to prevent this once we have a forked version of haddock
-- and can control the format of the interface files.
nameDef ∷ CabalInfo → Name → IO(Maybe Def)
nameDef info nm = do
  let modul = nameModule nm
      srcSpan = nameSrcSpan nm
      modName = moduleNameString $ moduleName modul
      nameStr = occNameString $ getOccName nm

  fnMay ← findModuleFile (Set.toList $ cabalSrcDirs info) $ modulePathFromName modName
  let fn = show $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  loc ← fromMaybe (fn,0,0) <$> srcSpanLoc fn srcSpan
  traceIO modName
  traceIO nameStr
  traceIO $ show loc
  return $ Just $ Def (splitOn "." modName, nameStr) nameStr Value loc

moduleDef ∷ CabalInfo → Haddock.InstalledInterface → IO Def
moduleDef info iface = do
  let modNm∷String = moduleNameString $ moduleName $ Haddock.instMod iface
      modPath = modulePathFromName modNm
  fnMay ← findModuleFile (Set.toList $ cabalSrcDirs info) modPath
  let fn = show $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  return $ Def modPath modNm Module (fn,0,0)

defsFromHaddock ∷ CabalInfo → Haddock.InstalledInterface → IO [Def]
defsFromHaddock info iface = do
  exportedDefs' ← mapM (nameDef info) $ Haddock.instExports iface
  let exportedDefs = catMaybes exportedDefs'
  modDef ← moduleDef info iface
  return $ modDef : exportedDefs

-- TODO Redirect stdout to stderr.
-- toStderr ∷ ∀a. Sh a → Sh a
-- toStderr = id

-- TODO escape ‘v’!
mkParam :: ∀m.(Monoid m,IsString m) ⇒ m → m → m
mkParam k v = "--" <> k <> "=" <> v <> "" -- Escape v!

-- runHandles ∷ FilePath → [Text] → [StdHandle] → (Handle→Handle→Handle→Sh a) → Sh a
-- transferLinesAndCombine :: Handle -> (Text -> IO ()) -> IO Text
  --let allToStderr in out err → 
      --cabal_ = runHandles "cabal" args [] allToStderr

graphCmd ∷ CabalInfo → IO Graph
graphCmd info = do
  let tmpfile = "/tmp/iface-file-for-srclib-haskell"∷Text
  let toStderr = log_stdout_with $ (Text.unpack >>> hPutStrLn stderr)
  let cabal_ = run_ "cabal"

  shelly $ toStderr $ do
    haddockPath ← fromMaybe (error "srclib-haddock is not installed!") <$>
      which "srclib-haddock"

    cd $ fromText $ fromString $ P.getPathString $ P.dropFileName $ cabalFile info
    cabal_ ["sandbox", "init", mkParam "sandbox" "/tmp/fuckyou"]
    cabal_ ["install", "--only-dependencies"]
    cabal_ ["configure", mkParam "builddir" "/tmp/fuckeverything"]
    cabal_ [ "haddock", "--executables", "--internal"
           , mkParam "with-haddock" $ toTextIgnore haddockPath
           , mkParam "haddock-options" ("-D" <> tmpfile)
           , mkParam "builddir" "/tmp/fuckeverything"
           ]

  ifaceFile ← either error id <$>
    Haddock.readInterfaceFile Haddock.freshNameCache (Text.unpack tmpfile)

  let ifaces = Haddock.ifInstalledIfaces ifaceFile
  haddockDefs ← mapM (defsFromHaddock info) ifaces
  return $ Graph $ traceShowId $ concat haddockDefs

depresolveCmd ∷ CabalInfo → IO [ResolvedDependency]
depresolveCmd = cabalDependencies >>> Set.toList >>> mapM resolve

dumpJSON ∷ ToJSON a ⇒ a → IO ()
dumpJSON = encode >>> BC.putStrLn

withSourceUnitFromStdin ∷ ToJSON a ⇒ (CabalInfo → IO a) → IO ()
withSourceUnitFromStdin proc = do
  unit ← JSON.decode <$> LBS.getContents
  maybe usage (proc >=> (encode>>>BC.unpack>>>traceIO)) unit
  maybe usage (proc >=> dumpJSON) unit

usage ∷ IO ()
usage = do
  progName ← Sys.getProgName
  putStrLn "Usage:"
  putStrLn $ concat ["    ", progName, " scan"]
  putStrLn $ concat ["    ", progName, " graph < sourceUnit"]
  putStrLn $ concat ["    ", progName, " depresolve < sourceUnit"]

srclibRun ∷ [String] → IO ()
srclibRun ("scan":_) = scanCmd >>= dumpJSON
srclibRun ["graph"] = withSourceUnitFromStdin graphCmd
srclibRun ["depresolve"] = withSourceUnitFromStdin depresolveCmd
srclibRun _ = usage

main ∷ IO ()
main = Sys.getArgs >>= srclibRun

test ∷ IO ()
test = quickCheck prop_cabalInfoJson
