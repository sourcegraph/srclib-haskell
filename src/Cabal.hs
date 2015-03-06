{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cabal ( Repo(..)
             , isCabalFile
             , RawDep
             , CabalInfo(..)
             , analyse, Warning(..)
             , toSrcUnit, fromSrcUnit
             , prop_srcUnitConversion
             , resolve
             , moduleMap
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
import Distribution.Version as Cabal
import Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Configuration as Cabal
import Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Text as Cabal

import qualified Locations as Loc
import Locations (RepoPath)
import qualified Srclib as Src

import Distribution.Hackage.DB (Hackage, readHackage)


-- Interface -----------------------------------------------------------------

data Repo = Repo
  { repoFiles ∷ Map RepoPath FilePath
  , repoCabalFiles ∷ Map RepoPath String
  }

data Warning = InvalidCabalFile RepoPath Text
  deriving Show

type RawDep = (Text,Text)

data CabalInfo = CabalInfo
  { cabalFile         ∷ RepoPath
  , cabalPkgName      ∷ Text
  , cabalPkgDir       ∷ RepoPath
  , cabalDependencies ∷ Map Text Text
  , cabalSrcFiles     ∷ Set RepoPath
  , cabalSrcDirs      ∷ Set RepoPath
  , cabalGlobs        ∷ Set Text
  , cabalRepoURI      ∷ Text
  , cabalRepoRev      ∷ Text
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
toSrcUnit (CabalInfo cf pkg pdir deps files dirs globs uri rev) =
  Src.SourceUnit cf pkg pdir (m deps) (s dirs) (s files) (s globs) uri rev
    where s = Set.toList
          m = M.toList

fromSrcUnit ∷ Src.SourceUnit → Maybe CabalInfo
fromSrcUnit (Src.SourceUnit cf pkg pdir deps files dirs globs uri rev) = Just $
  CabalInfo cf pkg pdir (m deps) (s dirs) (s files) (s globs) uri rev
    where s ∷ Ord a ⇒ [a] → Set a
          s = Set.fromList
          m = M.fromList

instance (Ord a,Arbitrary a) => Arbitrary(Set a) where
  arbitrary = Set.fromList <$> arbitrary

instance Arbitrary CabalInfo where
  arbitrary = CabalInfo <$> arbitrary <*> arbitrary <*> arbitrary
                        <*> (M.fromList <$> arbitrary) <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_srcUnitConversion ∷ CabalInfo → Bool
prop_srcUnitConversion ci = Just ci≡fromSrcUnit(toSrcUnit ci)


-- Implementation ------------------------------------------------------------

specialDepInfo ∷ RawDep → Maybe (Text,Text,Text)
specialDepInfo (n,_) = M.lookup n $ M.fromList $
 [("base"    ,("git.haskell.org/packages/base.git"    ,"4.7.0.1","ghc-7.8"))
 ,("ghc-prim",("git.haskell.org/packages/ghc-prim.git","0.3.1.0","ghc-7.8"))
 ]


moduleMap ∷ CabalInfo → IO (Map Loc.ModulePath Src.Pkg)
moduleMap info = do
  return M.empty
--  hack ← readHackage
--  let pkgs = catMaybes(lookupPackage hack <$>(M.toList(cabalDependencies info)))
--      flatten = Cabal.flattenPackageDescription
--      pModules = Cabal.library
--               ⋙ maybeToList
--               ⋙ fmap Cabal.exposedModules
--               ⋙ join
--               ⋙ fmap (Cabal.display ⋙ T.pack ⋙ Loc.parseModulePath)
--      pName = T.pack . Cabal.display . Cabal.pkgName . Cabal.package
--      flatPkgs = flatten <$> pkgs
--  return $ M.fromList $ join $ (\p → (,pName p) <$> pModules p) <$> flatPkgs

safeMax ∷ Map k v → Maybe (k,v)
safeMax m = if M.null m then Nothing else Just(M.findMax m)

lookupPackage ∷ Hackage → (Text,Text) → Maybe Cabal.GenericPackageDescription
lookupPackage hackage d@(nm,vrange) = snd <$> safeMax acceptableVersions
  where dep = Src.ResolvedDependency
        availableVersions ∷ Map Cabal.Version Cabal.GenericPackageDescription
        availableVersions = fromMaybe M.empty $ M.lookup (T.unpack nm) hackage
        acceptableVersions = M.filterWithKey (\k _→okVersion k) availableVersions
        okVersion ∷ Cabal.Version → Bool
        okVersion v = fromMaybe False $
          Cabal.withinRange v <$> Cabal.simpleParse (T.unpack vrange)

resolve ∷ Hackage → (Text,Text) → Src.ResolvedDependency
resolve hackage d@(nm,vrange) = L.head $ catMaybes[special,bestMatch,Just fallback]
  where dep = Src.ResolvedDependency
        special = (\(repo,v,ref)→dep d repo nm v ref) <$> specialDepInfo d
        fallback = dep d "" nm vrange ""
        bestMatch = join $ cabalDep <$> lookupPackage hackage d

        fromRepoInfo ∷ Cabal.Version → (Text,Text) → Src.ResolvedDependency
        fromRepoInfo v (uri,rev) = dep d uri nm (T.pack $ Cabal.display v) rev

        cabalDep ∷ Cabal.GenericPackageDescription→Maybe Src.ResolvedDependency
        cabalDep genDesc = fromRepoInfo version <$> (repoInfo $ desc)
          where version = Cabal.pkgVersion $ Cabal.package desc
                desc = Cabal.flattenPackageDescription genDesc

prToMaybe ∷ Cabal.ParseResult a → Maybe a
prToMaybe (Cabal.ParseFailed x) = traceShow x Nothing
prToMaybe (Cabal.ParseOk _ x) = Just x

cabalInfo ∷ Repo → RepoPath → String → Maybe CabalInfo
cabalInfo repo cabalFilePath content = do
  genPkgDesc ← prToMaybe $ parsePackageDescription content
  topLevelDir ← Loc.parent cabalFilePath
  let desc = flattenPackageDescription genPkgDesc
      (uri, rev) = fromMaybe ("","") $ repoInfo desc
      PackageName pkg = pkgName $ package desc
      allRepoFiles = M.keys $ repoFiles repo
      dirs = (topLevelDir <>) <$> sourceDirs desc
      relevantFiles = filter (\f → any (`Loc.contains` f) dirs) allRepoFiles
      sourceFiles = filter (Loc.ext ⋙ (∈ [Just "hs",Just "lhs"])) relevantFiles
  return CabalInfo { cabalFile = cabalFilePath
                   , cabalPkgName = T.pack pkg
                   , cabalPkgDir = topLevelDir
                   , cabalDependencies = allDeps desc
                   , cabalSrcFiles = set sourceFiles
                   , cabalSrcDirs = set dirs
                   , cabalGlobs = set $ glob <$> dirs
                   , cabalRepoURI = uri
                   , cabalRepoRev = rev
                   }
    where set ∷ Ord a ⇒ [a] → Set a
          set = Set.fromList
          glob (Loc.Repo(Loc.FP[])) = "**/*.hs"
          glob rp = Loc.srclibPath rp <> "/**/*.hs"

repoInfo ∷ PackageDescription → Maybe (Text,Text)
repoInfo desc = best $ catMaybes $ cvt <$> Cabal.sourceRepos desc
  where best ∷ [(Cabal.RepoKind, Text, Text)] → Maybe (Text,Text)
        best [] = Nothing
        best x = Just $ (\(_,b,c)→(b,c)) $ L.maximumBy cmp x
          where cmp a b = compare (n a) (n b)
                n (Cabal.RepoKindUnknown _,_,_) = 1 :: Int
                n (Cabal.RepoHead,_,_)          = 2 :: Int
                n (Cabal.RepoThis,_,_)          = 3 :: Int
        cvt ∷ Cabal.SourceRepo → Maybe (Cabal.RepoKind, Text, Text)
        cvt r = do
          let k = Cabal.repoKind r
          loc ← Cabal.repoLocation r
          rev ← case (Cabal.repoTag r, Cabal.repoBranch r, Cabal.repoType r) of
                  (Just tag, _          , _       ) → Just tag
                  (Nothing , Just branch, _       ) → Just branch
                  (Nothing , Nothing    , Just Git) → Just "master"
                  (Nothing , Nothing    , _       ) → Nothing
          return (k, T.pack loc, T.pack rev)

allDeps ∷ PackageDescription → Map Text Text
allDeps desc = M.fromList $ toRawDep <$> deps
  where deps = buildDepends desc ++ concatMap getDeps (allBuildInfo desc)
        toRawDep (Cabal.Dependency (PackageName nm) v) =
          (T.pack nm, T.pack $ Cabal.display v)
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
