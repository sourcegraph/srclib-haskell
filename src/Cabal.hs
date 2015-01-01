{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cabal ( Repo
             , CabalInfo(..)
             , analyse, Warning(..)
             , toSrcUnit, fromSrcUnit
             , prop_srcUnitConversion
             ) where

import ClassyPrelude hiding ((<.>), (</>))
import Prelude.Unicode
import Control.Category.Unicode
import Data.Monoid.Unicode

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T

import Distribution.Package as Cabal
import Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration as Cabal
import Distribution.PackageDescription.Parse as Cabal

import qualified Locations as Loc
import Locations (RepoPath)
import qualified Srclib as Src


-- Interface -----------------------------------------------------------------

type Repo = Map RepoPath String

data Warning = InvalidCabalFile RepoPath Text

data CabalInfo = CabalInfo
  { cabalFile         ∷ RepoPath
  , cabalPkgName      ∷ Text
  , cabalPkgDir       ∷ RepoPath
  , cabalDependencies ∷ Set Text
  , cabalSrcFiles     ∷ Set RepoPath
  , cabalSrcDirs      ∷ Set RepoPath
  , cabalGlobs        ∷ Set Text
  } deriving (Show, Eq)

analyse ∷ Repo → ([Warning], Map RepoPath CabalInfo)
analyse repo = ([], M.fromList $ catMaybes $ info <$> cabalFiles)
  where
    cabalFiles = M.toList $ M.filterWithKey (\k _→Just "cabal"≡Loc.ext k) repo
    info ∷ (RepoPath, String) → Maybe (RepoPath, CabalInfo)
    info (f,c) = do ci ← cabalInfo repo f c
                    return (f,ci)

toSrcUnit ∷ CabalInfo → Src.SourceUnit
toSrcUnit (CabalInfo cf pkg pdir deps files dirs globs) =
  Src.SourceUnit cf pkg pdir (unSet deps) (unSet files) (unSet dirs) (unSet globs)

fromSrcUnit ∷ Src.SourceUnit → Maybe CabalInfo
fromSrcUnit (Src.SourceUnit cf pkg pdir deps files dirs globs) =
  Just $ CabalInfo cf pkg pdir (toSet deps) (toSet files) (toSet dirs) (toSet globs)

prop_srcUnitConversion ∷ CabalInfo → Bool
prop_srcUnitConversion ci = Just ci≡fromSrcUnit(toSrcUnit ci)


-- Implementation ------------------------------------------------------------

prToMaybe ∷ Cabal.ParseResult a → Maybe a
prToMaybe (Cabal.ParseFailed _) = Nothing
prToMaybe (Cabal.ParseOk _ x) = Just x

toGlob ∷ RepoPath → Text
toGlob rp = Loc.srclibPath rp <> "/**/*.hs"

unSet ∷ Ord a ⇒ Set a → [a]
unSet = Set.toList

toSet ∷ Ord a ⇒ [a] → Set a
toSet = Set.fromList

cabalInfo ∷ Repo → RepoPath → String → Maybe CabalInfo
cabalInfo repo cabalFilePath content = do
  genPkgDesc ← prToMaybe $ parsePackageDescription content
  topLevelDir ← Loc.parent cabalFilePath
  let desc = flattenPackageDescription genPkgDesc
      PackageName pkg = pkgName $ package desc
      allRepoFiles = M.keys repo
      sourceFiles = filter (Loc.ext ⋙ (≡Just "hs")) allRepoFiles
      dirs = (topLevelDir <>) <$> sourceDirs desc
  return CabalInfo { cabalFile = cabalFilePath
                   , cabalPkgName = T.pack pkg
                   , cabalPkgDir = topLevelDir
                   , cabalDependencies = allDeps desc
                   , cabalSrcFiles = toSet sourceFiles
                   , cabalSrcDirs = toSet dirs
                   , cabalGlobs = toSet $ toGlob <$> dirs
                   }

allDeps ∷ PackageDescription → Set Text
allDeps desc = toSet $ toRawDep <$> deps
  where deps = buildDepends desc ++ concatMap getDeps (allBuildInfo desc)
        toRawDep (Cabal.Dependency (PackageName nm) _) = T.pack nm
        getDeps build = L.concat [ buildTools build
                                 , pkgconfigDepends build
                                 , targetBuildDepends build
                                 ]

sourceDirs ∷ PackageDescription → [Loc.RepoPath]
sourceDirs desc =
  catMaybes $ (fmap Loc.Repo . Loc.parseRelativePath) <$> librarySourceFiles⊕executableSourceFiles
    where librarySourceFiles =
            T.pack <$> concat (maybeToList $ (libBuildInfo ⋙ hsSourceDirs) <$> library desc)
          executableSourceFiles =
            T.pack <$> concat ((buildInfo ⋙ hsSourceDirs) <$> executables desc)
