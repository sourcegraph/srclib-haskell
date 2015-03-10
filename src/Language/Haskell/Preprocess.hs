{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Language.Haskell.Preprocess where


-- Imports -------------------------------------------------------------------

import BasicPrelude hiding (empty,find, Map, mapM, forM)
import Prelude.Unicode
import Control.Category.Unicode
import Turtle
import qualified Prelude

import qualified Filesystem.Path.CurrentOS as P
import qualified Control.Foldl as Fold
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe

import qualified Language.Preprocessor.Unlit as CPP
import qualified Language.Preprocessor.Cpphs as CPP

import qualified Data.List as L

import System.Posix.Process
import qualified System.IO.Temp as IO
import qualified System.IO as IO
import Control.DeepSeq

import qualified Distribution.Package as C
import qualified Distribution.Simple.Build.Macros as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Verbosity as C
import qualified Distribution.Version as C
import qualified Distribution.PackageDescription.Configuration as C
import qualified Language.Haskell.Extension as C

import Debug.Trace

import Text.Printf

import Language.Haskell.Preprocess.Macros

import System.Directory

import Data.Traversable


-- Types ---------------------------------------------------------------------

newtype ModuleName = MN { unModuleName ∷ Text }
  deriving (Eq,Ord,IsString,Show)

-- | Path's relative to the root of the source tree.
newtype SrcTreePath = STP { unSTP ∷ P.FilePath }
  deriving (Eq,Ord,IsString,Show,NFData)

instance NFData C.Extension

-- TODO Write a newtype for CabalFile
-- TODO Write a newtype for HSFile
-- TODO Write a newtype for SrcDir

data Pkg = Pkg {
  pkgRoot              ∷ !SrcTreePath,
  pkgCabalFile         ∷ !SrcTreePath,
  pkgModules           ∷ !(Map ModuleName SrcTreePath),
  pkgMacros            ∷ !String,
  pkgIncludeDirs       ∷ ![SrcTreePath],
  pkgDefaultExtensions ∷ ![C.Extension]
}


-- Values --------------------------------------------------------------------

stpStr ∷ SrcTreePath → String
stpStr = P.encodeString . unSTP

cabalFiles ∷ Pattern Text
cabalFiles = suffix ".cabal"

haskellFiles ∷ Pattern Text
haskellFiles = suffix ".hs" <|> suffix ".lhs"

consFold ∷ Fold a [a]
consFold = Fold.Fold (flip (:)) [] reverse

shellLines ∷ Shell a → IO [a]
shellLines = flip fold consFold

literateHaskellFilename ∷ P.FilePath → Bool
literateHaskellFilename fp = Just "lhs" ≡ P.extension fp

yuck ∷ String → String
yuck = T.pack
     ⋙ T.replace "defined(MIN_VERSION_hashable)" "1"
     ⋙ T.replace "defined(MIN_VERSION_integer_gmp)" "1"
     ⋙ T.unpack

-- TODO This uses deepseq to avoid issues related to lazy IO. I feel
--      like that's a bad idea.
processFile ∷ FilePath → Pkg → SrcTreePath → IO String
processFile root pkg fn = do
  let rootDir = root <> ""
  IO.withSystemTempFile "cabal_macros.h" $ \fp handle → do
    IO.hPutStrLn handle $ pkgMacros pkg
    IO.hPutStrLn handle ghcMacros
    IO.hClose handle

    let pstr = P.encodeString (rootDir <> unSTP fn)
    contents' ← Prelude.readFile pstr
    let contents = yuck contents'
    let defaults = CPP.defaultCpphsOptions
        cppOpts = defaults {
          CPP.preInclude = [fp],
          CPP.includes = stpStr <$> pkgIncludeDirs pkg,
          CPP.boolopts = (CPP.boolopts defaults) {
            CPP.hashline = False,
            CPP.stripC89 = True,
            CPP.literate = literateHaskellFilename(unSTP fn) }}
    noMacros ← CPP.runCpphs cppOpts pstr contents
    noMacros `deepseq` return noMacros

moduleName ∷ [SrcTreePath] → SrcTreePath → Maybe ModuleName
moduleName srcDirs fn = listToMaybe $ moduleNames
    where tryPrefix = flip P.stripPrefix $ unSTP fn
          pathToModule = P.splitDirectories
                       ⋙ fmap (T.filter (≠'/') . T.pack . P.encodeString)
                       ⋙ T.intercalate "."
                       ⋙ MN
          moduleNames = pathToModule . P.dropExtensions <$> pathNames
          pathNames = catMaybes $ tryPrefix . unSTP <$> srcDirs

-- TODO nub is not your friend.
-- TODO Handle parse failures!
allSourceDirs ∷ C.PackageDescription → [String]
allSourceDirs desc = nub $ join $ libDirs ++ exeDirs
  where
     libDirs = maybeToList (C.hsSourceDirs . C.libBuildInfo <$> C.library desc)
     exeDirs = C.hsSourceDirs . C.buildInfo <$> C.executables desc

-- TODO nub is not your friend.
-- TODO Handle parse failures!
-- TODO Copy-pasta!
allHeaderIncludeDirs ∷ C.PackageDescription → [String]
allHeaderIncludeDirs desc = nub $ join $ libDirs ++ exeDirs
  where
     libDirs = maybeToList (C.includeDirs . C.libBuildInfo <$> C.library desc)
     exeDirs = C.includeDirs . C.buildInfo <$> C.executables desc

-- TODO nub is not your friend.
-- TODO Handle parse failures!
-- TODO Copy-pasta!
allDefaultExtensions ∷ C.PackageDescription → [C.Extension]
allDefaultExtensions desc = nub $ join $ libDirs ++ exeDirs
  where
     libDirs = maybeToList (C.defaultExtensions . C.libBuildInfo <$> C.library desc)
     exeDirs = C.defaultExtensions . C.buildInfo <$> C.executables desc

-- chooseVersion chooses the greatest version that is explicitly mentioned.
chooseVersion ∷ C.VersionRange → C.Version
chooseVersion = C.foldVersionRange fallback id id id max max
  where fallback = C.Version [0,1,0,0] []

pkgDeps ∷ C.GenericPackageDescription → [C.Dependency]
pkgDeps gdesc = C.buildDepends desc
  where desc = allDeps gdesc
        allDeps = C.flattenPackageDescription
        justLibs gpd = C.flattenPackageDescription $ gpd
          { C.condTestSuites = []
          , C.condBenchmarks = []
          }

cabalMacros ∷ C.GenericPackageDescription → String
cabalMacros = C.generatePackageVersionMacros . pkgs
  where resolve (C.Dependency n v) = C.PackageIdentifier n $ chooseVersion v
        pkgs = fmap resolve . pkgDeps

cabalInfo ∷ FilePath → SrcTreePath → IO ([SrcTreePath],String,[SrcTreePath],[C.Extension])
cabalInfo root cabalFile = do
  traceM $ "cabalInfo " <> stpStr cabalFile
  let cabalFileStr = P.encodeString $ root <> unSTP cabalFile
  gdesc ← C.readPackageDescription C.normal cabalFileStr
  let desc        = C.flattenPackageDescription gdesc
      pkgRoot     = directory $ unSTP cabalFile
      dirStrs     = allSourceDirs desc
      incDirs     = allHeaderIncludeDirs desc
      exts        = allDefaultExtensions desc
      toSTP d     = STP $ P.collapse $ pkgRoot </> P.decodeString(d <> "")
      macros      = cabalMacros gdesc

  let result = (toSTP <$> dirStrs, macros, toSTP <$> incDirs, exts)
  result `deepseq` return result

stp ∷ FilePath → SrcTreePath
stp f = if P.absolute f
        then error(printf "`stp` called on absolute path: %s"(P.encodeString f))
        else STP $ P.collapse $ "./" <> f

scanPkg ∷ FilePath → SrcTreePath → IO Pkg
scanPkg root cabalFile = do
  let rootDir = root <> ""
  let pkgDir = P.directory $ unSTP cabalFile
  (srcDirs,macros,includeDirs,defaultExtensions) ← cabalInfo rootDir cabalFile

  hsFiles' ← shellLines (find haskellFiles $ rootDir <> pkgDir)
  let hsFiles = flip map hsFiles' $ \x →
        let dieWithError = error $ printf "%s is not a prefix of %s!"
                             (P.encodeString rootDir) (P.encodeString x)
        in stp $ fromMaybe dieWithError $ P.stripPrefix rootDir $ P.collapse x

      modules = M.fromList $ catMaybes $ flip map hsFiles $ \hs →
        (,hs) <$> moduleName srcDirs hs

  return $ Pkg (stp pkgDir) cabalFile modules macros includeDirs defaultExtensions

scan ∷ FilePath → IO (Set SrcTreePath)
scan root = do
  let rootDir = root <> ""
  packageFiles ← shellLines $ find cabalFiles rootDir
  return $ S.fromList $ flip map packageFiles $ \x →
    let dieWithError = error $ printf "%s is not a prefix of %s!"
                         (P.encodeString rootDir) (P.encodeString x)
    in stp $ fromMaybe dieWithError $ P.stripPrefix rootDir $ P.collapse x


type EntireProject = Map SrcTreePath (Pkg, Map ModuleName String)

loadPkg ∷ FilePath → SrcTreePath → IO (Pkg, Map ModuleName String)
loadPkg root pkgFile = do
  pkg ← scanPkg root pkgFile
  sources ← mapM (processFile root pkg) (pkgModules pkg)
  return (pkg,sources)

-- This loads the source code for all files into memory. It should only
-- be used on small projects and for debugging purposes.
loadEntireProject ∷ FilePath → IO EntireProject
loadEntireProject root = do
  pkgFiles ← S.toList <$> scan root
  mapM (loadPkg root) $ M.fromList $ (\x→(x,x)) <$> pkgFiles

loc ∷ FilePath → IO Int
loc root = do
  proj ← loadEntireProject root
  let allCode = L.concat $ join $ M.elems . snd <$> M.elems proj
  return $ length $ Prelude.lines $ allCode
