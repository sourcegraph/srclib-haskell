{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE NoImplicitPrelude    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import ClassyPrelude hiding ((</>),(<.>))

import qualified Locations as Loc
import           Srclib hiding (Graph,Def)

import           Control.Arrow
import           Control.Applicative

import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString.Lazy                          as LBS
import qualified Data.ByteString.Lazy.Char8                    as BC

import           Test.QuickCheck
import           Data.Aeson                                    as JSON
import           Data.List.Split

import qualified Distribution.Verbosity                        as Verbosity
import           Shelly hiding (FilePath,path,(</>),(<.>),canonicalize)
import qualified System.Directory                              as Sys
import qualified System.Environment                            as Sys
import           System.FilePath.Find                          ((&&?), (==?))
import qualified System.FilePath.Find                          as P
import           System.IO (hGetChar, hTell, IOMode(..))
import           System.IO.Error
import qualified System.Path                                   as P
import           System.Path ((</>),(<.>))
import           System.Posix.Process                          (getProcessID)

import           Distribution.Package                          as Cabal
import           Distribution.PackageDescription               as Cabal
import           Distribution.PackageDescription.Configuration as Cabal
import           Distribution.PackageDescription.Parse         as Cabal
import qualified Documentation.Haddock                         as Haddock
import           GHC
import           Name


-- Types ---------------------------------------------------------------------

type ModulePath = ([Text],Text)

-- All paths in a CabalInfo should be relative to the repository root.
data CabalInfo = CabalInfo
  { cabalFile         ∷ P.RelFile
  , cabalPkgName      ∷ Text
  , cabalDependencies ∷ Set Text
  , cabalSrcFiles     ∷ Set P.RelFile
  , cabalSrcDirs      ∷ Set P.RelDir
  } deriving (Show, Eq)

data Graph = Graph [Def]

data DefKind = Module | Value | Type
  deriving (Show)

data Def = Def ([Text],Text) Text DefKind (Text,Int,Int)

data Binding = Binding
  { bindModule ∷ ModulePath
  , bindName   ∷ Text
  , bindKind   ∷ DefKind
  , bindSpan   ∷ Loc.Span
  } deriving (Show)

findFiles ∷ P.FindClause Bool → P.AbsDir → IO [P.AbsFile]
findFiles q root = do
  let cond = P.fileType ==? P.RegularFile &&? q
  fileNames ← P.find P.always cond $ P.getPathString root
  return $ map P.asAbsPath fileNames

allDeps ∷ PackageDescription → Set Text
allDeps desc = Set.fromList $ toRawDep <$> deps
  where deps = buildDepends desc ++ concatMap getDeps (allBuildInfo desc)
        toRawDep (Cabal.Dependency (PackageName nm) _) = T.pack nm
        getDeps build = L.concat [ buildTools build
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
                   , cabalPkgName = T.pack name
                   , cabalDependencies = allDeps desc
                   , cabalSrcFiles = Set.fromList $ map (P.makeRelative repoDir) sourceFiles
                   , cabalSrcDirs = Set.fromList $ map (P.makeRelative repoDir) dirs
                   }

resolve ∷ Text → IO ResolvedDependency
resolve nm = return $ ResolvedDependency nm "" "" "" ""

modulePathFromName ∷ Text → ModulePath
modulePathFromName nm =
  case reverse $ T.splitOn "." nm of
    [] → error "Empty string is not a valid module name!"
    leaf:reversedPath → (reverse reversedPath, leaf)

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

-- TODO escape ‘v’!
mkParam :: ∀m.(Semigroup m,IsString m) ⇒ m → m → m
mkParam k v = "--" <> k <> "=" <> v <> "" -- Escape v!

defsFromHaddock ∷ CabalInfo → Haddock.InstalledInterface → IO [Def]
defsFromHaddock info iface = do
  exportedDefs' ← mapM (nameDef info) $ Haddock.instExports iface
  let exportedDefs = catMaybes exportedDefs'
  modDef ← moduleDef info iface
  return $ modDef : exportedDefs

findModuleFile = undefined

srcSpanSpan = undefined

getCabalInfo ∷ SourceUnit → CabalInfo
getCabalInfo = undefined

toSourceUnit ∷ CabalInfo → SourceUnit
toSourceUnit = undefined

instance ToJSON Graph where
 toJSON = undefined

instance ToJSON CabalInfo where
 toJSON = toJSON . toSourceUnit

moduleDef ∷ CabalInfo → Haddock.InstalledInterface → IO Def
moduleDef info iface = do
  let modNm = T.pack $ moduleNameString $ moduleName $ Haddock.instMod iface
      modPath = modulePathFromName modNm
  fnMay ← findModuleFile (Set.toList $ cabalSrcDirs info) modPath
  let fn = show $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  return $ Def modPath modNm Module (T.pack fn,0,0)

nameDef ∷ CabalInfo → Name → IO(Maybe Def)
nameDef info nm = do
  let modul = nameModule nm
      srcSpan = nameSrcSpan nm
      modName = T.pack $ moduleNameString $ moduleName modul
      nameStr = T.pack $ occNameString $ getOccName nm

  fnMay ← findModuleFile (Set.toList $ cabalSrcDirs info) $ modulePathFromName modName
  let fn = tshow $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  loc ← fromMaybe (fn,0,0) <$> srcSpanSpan fn srcSpan
  return $ Just $ Def (T.splitOn "." modName, nameStr) nameStr Value loc


-- Toolchain Command-Line Interface ------------------------------------------

graphCmd ∷ CabalInfo → IO Graph
graphCmd info = do
  pid ← toInteger <$> getProcessID

  let mktemp n = "/tmp/srclib-haskell-" <> n <> "." <> fromString(show pid)
      symbolGraph = mktemp "symbol-graph"
      sandbox = mktemp "sandbox"
      buildDir = mktemp "build-directory"

  let toStderr = log_stdout_with $ (T.unpack >>> hPutStrLn stderr)
  let cabal_ = run_ "cabal"

  shelly $ toStderr $ do
    haddockPath ← fromMaybe (error "srclib-haddock is not installed!") <$>
      which "srclib-haddock"

    cd $ fromText $ fromString $ P.getPathString $ P.dropFileName $ cabalFile info
    cabal_ ["sandbox", "init", mkParam "sandbox" sandbox]
    cabal_ ["install", "--only-dependencies"]
    cabal_ ["configure", mkParam "builddir" buildDir]
    cabal_ [ "haddock", "--executables", "--internal"
           , mkParam "with-haddock" $ toTextIgnore haddockPath
           , mkParam "haddock-options" ("-D" <> symbolGraph)
           , mkParam "builddir" buildDir
           ]

  ifaceFile ← either error id <$>
    Haddock.readInterfaceFile Haddock.freshNameCache (T.unpack symbolGraph)

  let ifaces = Haddock.ifInstalledIfaces ifaceFile
  haddockDefs ← mapM (defsFromHaddock info) ifaces
  return $ Graph $ concat haddockDefs

depresolveCmd ∷ CabalInfo → IO [ResolvedDependency]
depresolveCmd = cabalDependencies >>> Set.toList >>> mapM resolve

dumpJSON ∷ ToJSON a ⇒ a → IO ()
dumpJSON = encode >>> BC.putStrLn

withCabalInfoFromStdin ∷ ToJSON a ⇒ (CabalInfo → IO a) → IO ()
withCabalInfoFromStdin proc = do
  unit ← JSON.decode <$> LBS.getContents
  maybe usage (proc >=> dumpJSON) $ getCabalInfo <$> unit

usage ∷ IO ()
usage = do
  progName ← T.pack <$> Sys.getProgName
  putStrLn "Usage:"
  putStrLn $ T.concat ["    ", progName, " scan"]
  putStrLn $ T.concat ["    ", progName, " graph < sourceUnit"]
  putStrLn $ T.concat ["    ", progName, " depresolve < sourceUnit"]
  putStrLn progName

srclibRun ∷ [Text] → IO ()
srclibRun ("scan":_) = scanCmd >>= dumpJSON
srclibRun ["graph"] = withCabalInfoFromStdin graphCmd
srclibRun ["depresolve"] = withCabalInfoFromStdin depresolveCmd
srclibRun _ = usage

main ∷ IO ()
main = getArgs >>= srclibRun
