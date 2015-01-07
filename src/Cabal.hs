{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cabal ( Repo(..)
             , isCabalFile
             , CabalInfo(..)
             , analyse, Warning(..)
             , toSrcUnit, fromSrcUnit
             , prop_srcUnitConversion
             ) where

import ClassyPrelude hiding ((<.>), (</>))
import Prelude.Unicode
import Control.Category.Unicode
import Data.Monoid.Unicode
import Test.QuickCheck

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

data Repo = Repo
  { repoFiles ∷ Map RepoPath FilePath
  , repoCabalFiles ∷ Map RepoPath String
  }

data Warning = InvalidCabalFile RepoPath Text
  deriving Show

data CabalInfo = CabalInfo
  { cabalFile         ∷ RepoPath
  , cabalPkgName      ∷ Text
  , cabalPkgDir       ∷ RepoPath
  , cabalDependencies ∷ Set Text
  , cabalSrcFiles     ∷ Set RepoPath
  , cabalSrcDirs      ∷ Set RepoPath
  , cabalGlobs        ∷ Set Text
  } deriving (Show, Eq)

isCabalFile ∷ RepoPath → Bool
isCabalFile f = Loc.ext f ≡ Just "cabal"

analyse ∷ Repo → ([Warning], Map RepoPath CabalInfo)
analyse repo = ([], M.fromList $ catMaybes $ info <$> cabalFiles)
  where
    cabalFiles = M.toList $ repoCabalFiles repo
    info ∷ (RepoPath, String) → Maybe (RepoPath, CabalInfo)
    info (f,c) = do ci ← cabalInfo repo f c
                    return (f,ci)

toSrcUnit ∷ CabalInfo → Src.SourceUnit
toSrcUnit (CabalInfo cf pkg pdir deps files dirs globs) =
  Src.SourceUnit cf pkg pdir (u deps) (u dirs) (u files) (u globs)
    where u = Set.toList

fromSrcUnit ∷ Src.SourceUnit → Maybe CabalInfo
fromSrcUnit (Src.SourceUnit cf pkg pdir deps files dirs globs) =
  Just $ CabalInfo cf pkg pdir (toSet deps) (toSet dirs) (toSet files) (toSet globs)

instance (Ord a,Arbitrary a) => Arbitrary(Set a) where
  arbitrary = Set.fromList <$> arbitrary

instance Arbitrary CabalInfo where
  arbitrary = CabalInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary

prop_srcUnitConversion ∷ CabalInfo → Bool
prop_srcUnitConversion ci = Just ci≡fromSrcUnit(toSrcUnit ci)


-- Implementation ------------------------------------------------------------

prToMaybe ∷ Cabal.ParseResult a → Maybe a
prToMaybe (Cabal.ParseFailed x) = traceShow x Nothing
prToMaybe (Cabal.ParseOk _ x) = Just x

toGlob ∷ RepoPath → Text
toGlob (Loc.Repo(Loc.FP[])) = "**/*.hs"
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
      allRepoFiles = M.keys $ repoFiles repo
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
