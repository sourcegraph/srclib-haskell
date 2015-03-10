{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Language.Haskell.Preprocess where


-- Imports -------------------------------------------------------------------

import BasicPrelude hiding (empty,find, Map, mapM, forM, show)
import Prelude.Unicode
import Control.Category.Unicode
import Turtle
import qualified Prelude
import Prelude (show)

import Language.Haskell.Exts.Annotated as HSE hiding (ModuleName)
import Language.Haskell.Exts.Extension as HSE hiding (ModuleName)

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
instance NFData SrcSpanInfo
instance NFData a ⇒ NFData (Module a)

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

hackAroundCPPHSFlaws ∷ String → String
hackAroundCPPHSFlaws = T.pack
     ⋙ T.replace "defined(MIN_VERSION_hashable)" "1"
     ⋙ T.replace "defined(MIN_VERSION_integer_gmp)" "1"
     ⋙ T.unpack

prToMaybe ∷ ParseResult a → Maybe a
prToMaybe (ParseOk x) = Just x
prToMaybe (ParseFailed l c) =
  flip trace Nothing $ printf "Parse failed! %s %s" (show l) (show c)

cvtKExt ∷ C.KnownExtension → Maybe KnownExtension
cvtKExt e = case e of
  C.OverlappingInstances → Just OverlappingInstances
  C.UndecidableInstances → Just UndecidableInstances
  C.IncoherentInstances → Just IncoherentInstances
  C.InstanceSigs → Just InstanceSigs
  C.DoRec → Just DoRec
  C.RecursiveDo → Just RecursiveDo
  C.ParallelListComp → Just ParallelListComp
  C.MultiParamTypeClasses → Just MultiParamTypeClasses
  C.MonomorphismRestriction → Just MonomorphismRestriction
  C.FunctionalDependencies → Just FunctionalDependencies
  C.Rank2Types → Just Rank2Types
  C.RankNTypes → Just RankNTypes
  C.PolymorphicComponents → Just PolymorphicComponents
  C.ExistentialQuantification → Just ExistentialQuantification
  C.ScopedTypeVariables → Just ScopedTypeVariables
  C.PatternSignatures → Just PatternSignatures
  C.ImplicitParams → Just ImplicitParams
  C.FlexibleContexts → Just FlexibleContexts
  C.FlexibleInstances → Just FlexibleInstances
  C.EmptyDataDecls → Just EmptyDataDecls
  C.CPP → Just CPP
  C.KindSignatures → Just KindSignatures
  C.BangPatterns → Just BangPatterns
  C.TypeSynonymInstances → Just TypeSynonymInstances
  C.TemplateHaskell → Just TemplateHaskell
  C.ForeignFunctionInterface → Just ForeignFunctionInterface
  C.Arrows → Just Arrows
  C.Generics → Just Generics
  C.ImplicitPrelude → Just ImplicitPrelude
  C.NamedFieldPuns → Just NamedFieldPuns
  C.PatternGuards → Just PatternGuards
  C.GeneralizedNewtypeDeriving → Just GeneralizedNewtypeDeriving
  C.ExtensibleRecords → Just ExtensibleRecords
  C.RestrictedTypeSynonyms → Just RestrictedTypeSynonyms
  C.HereDocuments → Just HereDocuments
  C.MagicHash → Just MagicHash
  C.TypeFamilies → Just TypeFamilies
  C.StandaloneDeriving → Just StandaloneDeriving
  C.UnicodeSyntax → Just UnicodeSyntax
  C.UnliftedFFITypes → Just UnliftedFFITypes
  C.LiberalTypeSynonyms → Just LiberalTypeSynonyms
  C.TypeOperators → Just TypeOperators
  C.ParallelArrays → Just ParallelArrays
  C.RecordWildCards → Just RecordWildCards
  C.RecordPuns → Just RecordPuns
  C.DisambiguateRecordFields → Just DisambiguateRecordFields
  C.OverloadedStrings → Just OverloadedStrings
  C.GADTs → Just GADTs
  C.MonoPatBinds → Just MonoPatBinds
  C.RelaxedPolyRec → Just RelaxedPolyRec
  C.ExtendedDefaultRules → Just ExtendedDefaultRules
  C.UnboxedTuples → Just UnboxedTuples
  C.DeriveDataTypeable → Just DeriveDataTypeable
  C.ConstrainedClassMethods → Just ConstrainedClassMethods
  C.PackageImports → Just PackageImports
  C.LambdaCase → Just LambdaCase
  C.ImpredicativeTypes → Just ImpredicativeTypes
  C.NewQualifiedOperators → Just NewQualifiedOperators
  C.PostfixOperators → Just PostfixOperators
  C.QuasiQuotes → Just QuasiQuotes
  C.TransformListComp → Just TransformListComp
  C.ViewPatterns → Just ViewPatterns
  C.XmlSyntax → Just XmlSyntax
  C.RegularPatterns → Just RegularPatterns
  C.TupleSections → Just TupleSections
  C.GHCForeignImportPrim → Just GHCForeignImportPrim
  C.NPlusKPatterns → Just NPlusKPatterns
  C.DoAndIfThenElse → Just DoAndIfThenElse
  C.RebindableSyntax → Just RebindableSyntax
  C.ExplicitForAll → Just ExplicitForAll
  C.DatatypeContexts → Just DatatypeContexts
  C.MonoLocalBinds → Just MonoLocalBinds
  C.DeriveFunctor → Just DeriveFunctor
  C.DeriveGeneric → Just DeriveGeneric
  C.DeriveTraversable → Just DeriveTraversable
  C.DeriveFoldable → Just DeriveFoldable
  C.NondecreasingIndentation → Just NondecreasingIndentation
  C.InterruptibleFFI → Just InterruptibleFFI
  C.CApiFFI → Just CApiFFI
  C.ExplicitNamespaces → Just ExplicitNamespaces
  C.DataKinds → Just DataKinds
  C.PolyKinds → Just PolyKinds
  C.MultiWayIf → Just MultiWayIf
  C.SafeImports → Just SafeImports
  C.Safe → Just Safe
  C.Trustworthy → Just Trustworthy
  C.DefaultSignatures → Just DefaultSignatures
  C.ConstraintKinds → Just ConstraintKinds
  _ → flip trace Nothing $ printf "Unknown Extension: %s" (show e)

maybeUnknown ∷ C.KnownExtension → Maybe Extension → Extension
maybeUnknown x eM = fromMaybe (UnknownExtension(show x)) eM

cvtExt ∷ C.Extension → Extension
cvtExt (C.EnableExtension x)   = maybeUnknown x (EnableExtension  <$> cvtKExt x)
cvtExt (C.DisableExtension x)  = maybeUnknown x (DisableExtension <$> cvtKExt x)
cvtExt (C.UnknownExtension nm) = UnknownExtension nm

extensionsHack ∷ [Extension]
extensionsHack =
  [ EnableExtension NPlusKPatterns
  , EnableExtension ExplicitForAll
  , EnableExtension FlexibleContexts
  , EnableExtension ExplicitNamespaces
  , EnableExtension CPP
  , EnableExtension MagicHash
  , EnableExtension TemplateHaskell
  , EnableExtension MultiParamTypeClasses
  , EnableExtension ExistentialQuantification
  ]

parseCode ∷ Pkg → FilePath → String → Maybe (Module SrcSpanInfo)
parseCode pkg filePath source = do
  let fileName = P.encodeString filePath
  let mode = defaultParseMode
        { parseFilename     = fileName
        , fixities          = Nothing
        , ignoreLinePragmas = False
        , extensions        = extensionsHack
                           ++ (cvtExt <$> pkgDefaultExtensions pkg)
                           ++ extensions defaultParseMode
        }
  prToMaybe $ parseFileContentsWithMode mode source

-- TODO This uses deepseq to avoid issues related to lazy IO. I feel
--      like that's a bad idea.
processFile ∷ NFData a => FilePath → Pkg → (String → a) → SrcTreePath → IO a
processFile root pkg transform fn = do
  let rootDir = root <> ""
  IO.withSystemTempDirectory "cpp_macros" $ \tmpDir → do
    let tmpDirFP = P.decodeString tmpDir
        cabalMacrosFP = (tmpDirFP </> "cabal_macros.h")
        compilerMacrosFP = (tmpDirFP </> "ghc_macros.h")

    writeFile cabalMacrosFP                  $ T.pack (pkgMacros pkg)
    writeFile compilerMacrosFP               $ T.pack compilerMacros
    writeFile (tmpDirFP </> "MachDeps.h")    $ T.pack machDeps
    writeFile (tmpDirFP </> "ghcautoconf.h") $ T.pack ghcAutoConf

    let pstr = P.encodeString (rootDir <> unSTP fn)
    contents ← hackAroundCPPHSFlaws <$> Prelude.readFile pstr

    let pkgIncludes = P.encodeString . (root </>) . unSTP <$> pkgIncludeDirs pkg

    let defaults = CPP.defaultCpphsOptions
        cppOpts = defaults {
          CPP.preInclude = P.encodeString <$> [compilerMacrosFP, cabalMacrosFP],
          CPP.includes = pkgIncludes ++ [tmpDir],
          CPP.boolopts = (CPP.boolopts defaults) {
            CPP.hashline = False,
            CPP.stripC89 = True,
            CPP.warnings = True,
            CPP.literate = literateHaskellFilename(unSTP fn) }}
    noMacros ← CPP.runCpphs cppOpts pstr contents
    let result = transform noMacros
    result `deepseq` return result

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


type EntireProject a = Map SrcTreePath (Pkg, Map ModuleName a)

loadPkg ∷ NFData a => FilePath → (Pkg → FilePath → String → a) → SrcTreePath → IO (Pkg, Map ModuleName a)
loadPkg root transform pkgFile = do
  pkg ← scanPkg root pkgFile
  sources ← forM (pkgModules pkg) $ \fp →
    processFile root pkg (transform pkg $ unSTP fp) fp
  return (pkg,sources)

-- This loads the source code for all files into memory. It should only
-- be used on small projects and for debugging purposes.
loadEntireProject ∷ NFData a ⇒ FilePath → (Pkg → FilePath → String → a) → IO (EntireProject a)
loadEntireProject root transform = do
  pkgFiles ← S.toList <$> scan root
  mapM (loadPkg root transform) $ M.fromList $ (\x→(x,x)) <$> pkgFiles

parseEntireProject ∷ FilePath → IO (EntireProject(Maybe(Module SrcSpanInfo)))
parseEntireProject root = do
  pkgFiles ← S.toList <$> scan root
  mapM (loadPkg root parseCode) $ M.fromList $ (\x→(x,x)) <$> pkgFiles

-- This version loads the entire project into memory before counting lines.
loc ∷ FilePath → IO Int
loc root = do
  proj ← loadEntireProject root (\_ _ x → x)
  let allCode = L.concat $ join $ M.elems . snd <$> M.elems proj
  return $ length $ Prelude.lines $ allCode

-- This version only doesn't keep source code in memory.
lowMemLoc ∷ FilePath → IO Int
lowMemLoc root = do
  pkgFiles ← S.toList <$> scan root
  fmap sum $ forM pkgFiles $ \pkgFile → do
    pkg ← scanPkg root pkgFile
    let modules = pkgModules pkg
    sum <$> forM (M.elems(pkgModules pkg)) (processFile root pkg (length . Prelude.lines))
