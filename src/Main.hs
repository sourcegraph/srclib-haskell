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
import           Data.Maybe
import           Distribution.Package                          as Cabal
import           Distribution.PackageDescription               as Cabal
import           Distribution.PackageDescription.Configuration as Cabal
import           Distribution.PackageDescription.Parse         as Cabal
import qualified Distribution.Verbosity                        as Verbosity
import           System.Directory                              (getCurrentDirectory)
import           System.Environment                            (getArgs,
                                                                getProgName)
import           System.FilePath.Find                          as Path
import           System.Path                                   as Path hiding (FilePath)
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
   { cabalFile         ∷ RelFile
   , cabalPkgName      ∷ String
   , cabalDependencies ∷ [RawDependency]
   , cabalSrcFiles     ∷ [RelFile]
   , cabalSrcDirs      ∷ [RelDir]
   } deriving (Show,Eq)

instance FromJSON CabalInfo where
 parseJSON (Object v) = do
    infoObj ← v .: "Data"
    case infoObj of
      Object info → do
        path ← asRelFile <$> info .: "Path"
        name ← v .: "Name"
        deps ← map RawDependency <$> v .: "Dependencies"
        files ← map asRelFile <$> v .: "Files"
        dirs ← map asRelDir <$> info .: "Dirs"
        return $ CabalInfo path name deps files dirs
      _ → mzero
 parseJSON _ = mzero

instance ToJSON CabalInfo where
  toJSON (CabalInfo path name deps files dirs) =
    let dir = dropFileName path in
      object [ "Type" .= ("HaskellPackage"∷String)
             , "Ops" .= object ["graph" .= Null, "depresolve" .= Null]
             , "Name" .= name
             , "Dir" .= getPathString dir
             , "Globs" .= map (getPathString >>> (++ "/**/*.hs")) dirs
             , "Files" .= map getPathString files
             , "Dependencies" .= deps
             , "Data" .= object [ "Path" .= getPathString path
                                , "Dirs" .= map getPathString dirs
                                ]
             , "Repo" .= Null
             , "Config" .= Null
             ]

-- TODO pathtype's Gen instances output far too much data. Hack around it.
newtype PathHack a b = PathHack (Path a b)
instance Arbitrary (PathHack a b) where
  arbitrary = return $ PathHack $ asPath "./asdf"

unPathHack ∷ PathHack a b → Path a b
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

data Graph = Graph ()

instance ToJSON Graph where
  toJSON (Graph ()) = object ["Docs".=e, "Refs".=e, "Defs".=e]
    where e = []∷[String]


-- Scaning Repos and Parsing Cabal files -------------------------------------

findFiles ∷ FindClause Bool → AbsDir → IO [AbsFile]
findFiles q root = do
  fileNames ← find always (fileType ==? RegularFile &&? q) $ getPathString root
  return $ map asAbsPath fileNames

allDeps ∷ PackageDescription → [RawDependency]
allDeps desc = map toRawDep deps
  where deps = buildDepends desc ++ (concat $ map getDeps $ allBuildInfo desc)
        toRawDep (Cabal.Dependency (PackageName nm) _) = RawDependency nm
        getDeps build = concat [ buildTools build
                               , pkgconfigDepends build
                               , targetBuildDepends build
                               ]

sourceDirs ∷ PackageDescription → [RelDir]
sourceDirs desc = map asRelDir $ librarySourceFiles ++ executableSourceFiles
  where librarySourceFiles =
          concat $ maybeToList $ (libBuildInfo>>>hsSourceDirs) <$> library desc
        executableSourceFiles =
          concat $ (buildInfo>>>hsSourceDirs) <$> executables desc

readCabalFile ∷ AbsDir → AbsFile → IO CabalInfo
readCabalFile repoDir cabalFilePath = do
  genPkgDesc ← readPackageDescription Verbosity.deafening $ show cabalFilePath
  let desc = flattenPackageDescription genPkgDesc
      PackageName name = pkgName $ package desc
      dirs = map (combine $ takeDirectory cabalFilePath) $ sourceDirs desc

  sourceFiles ← concat <$> (sequence $ map(findFiles $ extension==?".hs") dirs)
  return $ CabalInfo { cabalFile = makeRelative repoDir cabalFilePath
                     , cabalPkgName = name
                     , cabalDependencies = allDeps desc
                     , cabalSrcFiles = map (makeRelative repoDir) sourceFiles
                     , cabalSrcDirs = map (makeRelative repoDir) dirs
                     }


-- Resolve Dependencies ------------------------------------------------------

resolve ∷ RawDependency → IO ResolvedDependency
resolve (RawDependency d) = return (ResolvedDependency d)


-- Toolchain Command-Line Interface ------------------------------------------

scanCmd ∷ IO [CabalInfo]
scanCmd = do
  cwd ← getCurrentDirectory
  let root = asAbsDir cwd
  cabalFiles ← findFiles (extension ==? ".cabal") root
  sequence $ map (readCabalFile root) cabalFiles

graphCmd ∷ CabalInfo → IO Graph
graphCmd _ = return $ Graph ()

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
  cmd ← getProgName
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
main = getArgs >>= run

test ∷ IO ()
test = quickCheck prop_cabalInfoJson
