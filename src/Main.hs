-- TODO I'm using flattenPackageDescription because it's easy, but it might
--      not consistently give correct results.
-- TODO The scan command currently doesn't output enough dependency
--      information. This isn't technically incorrect, but it will need to
--      be fixed when the depresolve command is implemented.
-- TODO What's left?
--  Pull packages from hackage in order to get better depresolve output.
--  Figure out the best way to graph Haskell code.
--    How does Hoogle do it?
--    How does Hackage do it?
--  Next steps:
--    Read Hackage and Hoogle, and see how they approach things.

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Aeson                                    as JSON
import qualified Data.ByteString.Lazy                          as LBS
import qualified Data.ByteString.Lazy.Char8                    as BC
import           Data.List.Split
import           Data.Maybe
import           Distribution.Package                          as Cabal
import           Distribution.PackageDescription               as Cabal
import           Distribution.PackageDescription.Configuration as Cabal
import           Distribution.PackageDescription.Parse         as Cabal
import qualified Distribution.Verbosity                        as Verbosity
import           Symbols
import qualified System.Directory                              as Sys
import qualified System.Environment                            as Sys
import           System.FilePath.Find                          ((&&?), (==?))
import qualified System.FilePath.Find                          as P
import           System.IO
import           System.IO.Error
import qualified System.Path                                   as P
import           Test.QuickCheck


-- Dependencies --------------------------------------------------------------

data RawDependency = RawDependency String deriving (Show,Eq)
data ResolvedDependency = ResolvedDependency String deriving (Show,Eq)

instance ToJSON RawDependency where
  toJSON (RawDependency d) = toJSON d

rawDependency ∷ ResolvedDependency → RawDependency
rawDependency (ResolvedDependency d) = (RawDependency d)

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

-- File name, column number, and column number. These start at 1 (not 0),
-- and are based on unicode characters (not bytes).
type Loc = (FilePath,Integer,Integer)
data Def = Def { def_module ∷ [String]
               , def_name ∷ String
               , def_kind ∷ SymbolKind
               , def_loc ∷ Loc
               } deriving Show

data Graph = Graph [Def]

instance ToJSON Def where
  toJSON d = object [ "Path" .= (joinL "/" $ def_module d)
                    , "TreePath" .= (joinL "/" $ def_module d)
                    , "Name" .= def_name d
                    , "Kind" .= (show $ def_kind d)
                    , "File" .= (case def_loc d of (fn,_,_)→fn)
                    , "DefStart" .= (case def_loc d of (_,s,_)→s)
                    , "DefEnd" .= (case def_loc d of (_,_,e)→e)
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
  where deps = buildDepends desc ++ (concat $ map getDeps $ allBuildInfo desc)
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

  sourceFiles ← concat <$> (sequence $ map(findFiles$P.extension==?".hs") dirs)
  return $ CabalInfo { cabalFile = P.makeRelative repoDir cabalFilePath
                     , cabalPkgName = name
                     , cabalDependencies = allDeps desc
                     , cabalSrcFiles = map (P.makeRelative repoDir) sourceFiles
                     , cabalSrcDirs = map (P.makeRelative repoDir) dirs
                     }


-- Resolve Dependencies ------------------------------------------------------

resolve ∷ RawDependency → IO ResolvedDependency
resolve (RawDependency d) = return (ResolvedDependency d)


-- Graph Symbol References ---------------------------------------------------

-- type SrcPtr = (Int,Int)
-- data SymbolInfo = SymbolInfo { si_nm∷String, si_loc∷(String,SrcPtr,SrcPtr) }
-- findSymbols ∷ (MonadIO m) ⇒ [FilePath] → FilePath → m [SymbolInfo]

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

mkDef ∷ String → SymbolInfo → IO Def
mkDef moduleName (SymbolInfo nm k (fn,start,end)) = do
  startOffset ← getLocOffset fn start
  endOffset ← getLocOffset fn end
  return $ Def (splitOn "." moduleName) nm k (fn,startOffset,endOffset)

graph ∷ FilePath → IO [Def]
graph fn = do
  (moduleName,symbols) ← findSymbols [] fn
  defs ← sequence $ mkDef moduleName <$> symbols
  -- (encode >>> BC.putStrLn) defs
  return $ defs


-- Toolchain Command-Line Interface ------------------------------------------

scanCmd ∷ IO [CabalInfo]
scanCmd = do
  cwd ← Sys.getCurrentDirectory
  let root = P.asAbsDir cwd
  cabalFiles ← findFiles (P.extension ==? ".cabal") root
  sequence $ map (readCabalFile root) cabalFiles

graphCmd ∷ CabalInfo → IO Graph
graphCmd info = do
  let files = cabalSrcFiles info
  defs ← concat <$> (sequence $ (P.getPathString>>>graph) <$> files)
  return $ Graph defs

depresolveCmd ∷ CabalInfo → IO [ResolvedDependency]
depresolveCmd = cabalDependencies >>> map resolve >>> sequence

dumpJSON ∷ ToJSON a ⇒ a → IO ()
dumpJSON = encode >>> BC.putStrLn

withSourceUnitFromStdin ∷ ToJSON a ⇒ (CabalInfo → IO a) → IO ()
withSourceUnitFromStdin proc = do
  unit ← JSON.decode <$> LBS.getContents
  maybe usage (\u → proc u >>= dumpJSON) unit

usage ∷ IO ()
usage = do
  cmd ← Sys.getProgName
  putStrLn "Usage:"
  putStrLn $ concat ["    ", cmd, " scan"]
  putStrLn $ concat ["    ", cmd, " graph < sourceUnit"]
  putStrLn $ concat ["    ", cmd, " depresolve < sourceUnit"]

run ∷ [String] → IO ()
run ("scan":_) = scanCmd >>= dumpJSON
run ["graph"] = withSourceUnitFromStdin graphCmd
run ["depresolve"] = withSourceUnitFromStdin depresolveCmd
run _ = usage

main ∷ IO ()
main = Sys.getArgs >>= run

test ∷ IO ()
test = quickCheck prop_cabalInfoJson
