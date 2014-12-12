{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Shelly hiding (FilePath,path)

import           Data.String

import           Debug.Trace
import qualified Distribution.Verbosity                        as Verbosity
import           Test.QuickCheck

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

import           FastString
import           GHC
import           Name


-- Dependencies --------------------------------------------------------------

data RawDependency = RawDependency String deriving (Show,Eq)
data ResolvedDependency = ResolvedDependency String deriving (Show,Eq)

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

instance Arbitrary RawDependency where
  arbitrary = RawDependency <$> arbitrary


-- Source Units --------------------------------------------------------------

-- All paths are relative to repository root.
data CabalInfo = CabalInfo
   { cabalFile         ∷ P.RelFile
   , cabalPkgName      ∷ String
   , cabalDependencies ∷ [RawDependency]
   , cabalSrcFiles     ∷ [P.RelFile]
   , cabalSrcDirs      ∷ [P.RelDir]
   } deriving (Show,Eq)


instance FromJSON CabalInfo where
 parseJSON (Object v) = do
    infoObj ← v .: "Data"
    case infoObj of
      Object info → do
        path ← P.asRelFile <$> info .: "Path"
        name ← v .: "Name"
        deps ← map RawDependency <$> v .: "Dependencies"
        files ← map P.asRelFile <$> v .: "Files"
        dirs ← map P.asRelDir <$> info .: "Dirs"
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
             , "Globs" .= map (P.getPathString >>> (++ "/**/*.hs")) dirs
             , "Files" .= map P.getPathString files
             , "Dependencies" .= deps
             , "Data" .= object [ "Path" .= P.getPathString path
                                , "Dirs" .= map P.getPathString dirs
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
    files ← map unPathHack <$> arbitrary
    dirs ← map unPathHack <$> arbitrary
    deps ← arbitrary
    name ← arbitrary
    return $ CabalInfo file name deps files dirs

prop_cabalInfoJson ∷ CabalInfo → Bool
prop_cabalInfoJson c = (Just c==) $ JSON.decode $ JSON.encode c


-- Source Graph --------------------------------------------------------------

-- TODO Making these both unsigned and making the second number a size would
--     make invalid ranges unrepresentables.

-- Loc is a filename with a span formed by two byte offsets.
type Loc = (FilePath,Integer,Integer)

data DefKind = Module | Value | Type
  deriving Show

data Def = Def { defModule ∷ [String]
               , defName   ∷ String
               , defKind   ∷ DefKind
               , defLoc    ∷ Loc
               } deriving Show

data Graph = Graph [Def]

joinL ∷ ∀a. [a] → [[a]] → [a]
joinL sep = concat <<< intersperse sep

instance ToJSON Def where
  toJSON d = object [ "Path" .= joinL "/" (defModule d)
                    , "TreePath" .= joinL "/" (defModule d)
                    , "Name" .= defName d
                    , "Kind" .= show (defKind d)
                    , "File" .= (case defLoc d of (fn,_,_)→fn)
                    , "DefStart" .= (case defLoc d of (_,s,_)→s)
                    , "DefEnd" .= (case defLoc d of (_,_,e)→e)
                    , "Exported" .= True
                    , "Test" .= False
                    , "JsonText" .= object[]
                    ]

instance ToJSON Graph where
  toJSON (Graph defs) = object ["Docs".=e, "Refs".=e, "Defs".=defs]
    where e = []∷[String]


-- Scaning Repos and Parsing Cabal files -------------------------------------

findFiles ∷ P.FindClause Bool → P.AbsDir → IO [P.AbsFile]
findFiles q root = do
  let cond = P.fileType ==? P.RegularFile &&? q
  fileNames ← P.find P.always cond $ P.getPathString root
  return $ map P.asAbsPath fileNames

allDeps ∷ PackageDescription → [RawDependency]
allDeps desc = map toRawDep deps
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
                   , cabalSrcFiles = map (P.makeRelative repoDir) sourceFiles
                   , cabalSrcDirs = map (P.makeRelative repoDir) dirs
                   }


-- Resolve Dependencies ------------------------------------------------------

resolve ∷ RawDependency → IO ResolvedDependency
resolve (RawDependency d) = return (ResolvedDependency d)


-- Graph Symbol References ---------------------------------------------------

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

srcSpanLoc ∷ SrcSpan → IO (Maybe Loc)
srcSpanLoc (UnhelpfulSpan _) = return Nothing
srcSpanLoc (RealSrcSpan r) = do
  let l1 = srcSpanStartLine r
      c1 = srcSpanStartCol r
      l2 = srcSpanEndLine r
      c2 = srcSpanEndCol r
      fn = unpackFS $ srcSpanFile r
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
nameDef ∷ Name → IO(Maybe Def)
nameDef nm = do
  let modul = nameModule nm
      srcSpan = nameSrcSpan nm
      modName = moduleNameString $ moduleName modul
      nameStr = occNameString $ getOccName nm
  loc ← fromMaybe ("<unknown>",0,0) <$> srcSpanLoc srcSpan
  traceIO modName
  traceIO nameStr
  traceIO $ show loc
  return $ Just $ Def (splitOn "." modName ++ [nameStr]) nameStr Value loc

moduleDef ∷ Haddock.InstalledInterface → Def
moduleDef iface =
  let modNm = (moduleNameString $ moduleName $ Haddock.instMod iface)∷String
  in Def (splitOn "." modNm) modNm Module (Haddock.instOrigFilename iface,0,0)

-- TODO I think we'll need the CabalInfo argument to rebase file paths from
-- the directory the cabal file is in, up to the directory at the root of
-- the file information from Haddock at this point.
-- repo. However, we are not correctly getting
defsFromHaddock ∷ CabalInfo → Haddock.InstalledInterface → IO [Def]
defsFromHaddock _ iface = do
  exportedDefs' ← mapM nameDef $ Haddock.instExports iface
  let exportedDefs = catMaybes exportedDefs'
  return $ moduleDef iface : exportedDefs

-- TODO Redirect stdout to stderr.
toStderr ∷ ∀a. Sh a → Sh a
toStderr = id

-- TODO escape ‘v’!
mkParam :: ∀m.(Monoid m,IsString m) ⇒ m → m → m
mkParam k v = "--" <> k <> "=" <> v <> "" -- Escape v!

graphCmd ∷ CabalInfo → IO Graph
graphCmd info = do
  let tmpfile = "/tmp/iface-file-for-srclib-haskell"∷Text
  let cabal_ = run_ "cabal" >>> silently

  shelly $ do
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
depresolveCmd = cabalDependencies >>> map resolve >>> sequence

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
