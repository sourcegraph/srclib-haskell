{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE NoImplicitPrelude    #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Haddock where

import           ClassyPrelude hiding ((</>), (<.>), maximumBy)
import           Prelude.Unicode
import           Control.Category.Unicode
import qualified Imports as Imp

import qualified Data.IntMap as IntMap
import qualified Data.Maybe as May
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.List as L
import           Data.Foldable (maximumBy)

import           Shelly hiding (FilePath, path, (</>), (<.>), canonicalize, trace)
import qualified Shelly
import qualified Filesystem.Path.CurrentOS as Path
import           System.Posix.Process (getProcessID)
import           Text.Regex.TDFA

import qualified Cabal as C
import           Locations as Loc
import qualified Srclib as Src

import Distribution.Hackage.DB (readHackage)


-- Types ---------------------------------------------------------------------

data Def = Def ModulePath Text Src.Kind Span
type PkgName = Text
type RepoURI = Text
type ModuleLookup = ModulePath → (Src.URI,Src.Pkg)
type SrcLocLookup = (String,Int,Int) → Int


-- Values --------------------------------------------------------------------

mkIndex ∷ Ord k ⇒ (a → k) → [a] → Map k a
mkIndex f = map (\r→(f r,r)) ⋙ M.fromList

composeN ∷ Int → (a → a) → (a → a)
composeN 0 _ = id
composeN 1 f = f
composeN n f = composeN (n-1) (f.f)

srcPath ∷ (RepoPath, SrcPath, FileShape) → SrcPath
srcPath (_,s,_) = s

fudgeTmpFile ∷ PathDB → String → (RepoPath, SrcPath, FileShape)
fudgeTmpFile (db,tmps) tmp = chooseFrom matches
  where modul = M.elems $ M.filterWithKey (\k v → looksLike tmp k) tmps
        looksLike a b = (a `L.isInfixOf` b) ∨ (b `L.isInfixOf` a)
        q m = Set.filter (srcPath ⋙ srcPathMatch m) db
        matches = Set.unions $ q <$> modul
        longest (_,Src(FP f),_) (_,Src(FP φ),_) = (compare `on` length) f φ
        fallback = (Repo(FP["bogus"]), Src(FP["Bogus"]), Shape [] IntMap.empty)
        chooseFrom s = if Set.null s then fallback else maximumBy longest s

pdbSourceFileNames ∷ PathDB → [Text]
pdbSourceFileNames (srcFiles, _) = fnStr <$> Set.toList srcFiles
  where fnStr (repo, src, _) = srclibPath $ Loc.srcToRepo repo src

tmpFileModule ∷ PathDB → String → ModulePath
tmpFileModule db tmp = fileToModulePath $ srcPath $ fudgeTmpFile db tmp

isVersionNumber ∷ Text → Bool
isVersionNumber = T.unpack ⋙ (=~ ("^([0-9]+\\.)*[0-9]+$"::String))

fudgePkgName ∷ Text → Text
fudgePkgName pkg =
  T.intercalate "-" $ reverse $ case reverse $ T.split (≡'-') pkg of
    []                 → []
    full@(last:before) → if isVersionNumber last then before else full

baseRepo = "github.com/bsummer4/packages-base"

safeHead ∷ [a] → Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

-- TODO Drops duplicated refs and defs. This is a quick hack, but IT IS WRONG.
--   The issue is that haskell allows multiple definitions for the same
--   things. For example, a type declaration and two defintions that
--   handle different pattern matches are all definitions from the perspective
--   of the grapher.
fudgeGraph ∷ Src.Graph → Src.Graph
fudgeGraph (Src.Graph defs refs) = Src.Graph (fudgeDefs defs) (fudgeRefs refs)
  where fudgeBy ∷ Ord b ⇒ (a → b) → [a] → [a]
        fudgeBy f = M.elems . M.fromList . map (\x→(f x,x))
        fudgeDefs ∷ [Src.Def] → [Src.Def]
        fudgeDefs = fudgeBy Src.defPath
        fudgeRefs ∷ [Src.Ref] → [Src.Ref]
        fudgeRefs = fudgeBy $ Src.refStart &&& Src.refEnd

pos ∷ (Int,Int,Text) → Text
pos (x,y,nm) = tshow x <> ":" <> tshow y <> " (" <> nm

summary ∷ Src.Graph → Text
summary (Src.Graph dfs rfs) =
  unlines("[refs]" : (pos<$>refs)) <> unlines("[defs]" : (pos<$>defs))
    where defs = (\x→(Src.defDefStart x, Src.defDefEnd x, tshow $ Src.defPath x)) <$> dfs
          refs = (\x→(Src.refStart x, Src.refEnd x, tshow $ Src.refDefPath x)) <$> rfs

convertGraph ∷ a → b → c → d → e → Src.Graph
convertGraph x y z p r = mempty

instance Semigroup Shelly.FilePath where
  a <> b = a `mappend` b

repoMap ∷ C.CabalInfo → IO (Map PkgName RepoURI)
repoMap info = do
  hack ← readHackage
  ds ← mapM(C.resolve hack ⋙ return) $ M.toList $ C.cabalDependencies info
  return $ M.fromList $ (\d → (Src.depToUnit d, Src.depToRepoCloneURL d)) <$> ds

convertModuleGraph ∷ ModuleLookup → SrcLocLookup → [(Text,[Imp.ModuleRef])] → Src.Graph
convertModuleGraph toRepoAndPkg toOffset refMap =
  Src.Graph [] $ concat $ fmap cvt . snd <$> refMap
    where repoFile = Repo . fromMaybe mempty . Loc.parseRelativePath . T.pack
          cvt ∷ Imp.ModuleRef → Src.Ref
          cvt (fn,(sl,sc),(el,ec),mp) =
           let (repo,pkg) = toRepoAndPkg mp
           in Src.Ref
                { Src.refDefRepo     = repo
                , Src.refDefUnitType = "HaskellPackage"
                , Src.refDefUnit     = pkg
                , Src.refDefPath     = Src.PModule pkg mp
                , Src.refIsDef       = False
                , Src.refFile        = repoFile fn
                , Src.refStart       = toOffset (fn,sl,sc)
                , Src.refEnd         = toOffset (fn,el,ec)
                }

_3_2 ∷ (a,b,c) → b
_3_2 (_,b,_) = b

ourModules ∷ PathDB → [(RepoPath,ModulePath)]
ourModules = fmap f . Set.toList . fst
  where f (a,b,c) = (srcToRepo a b, fileToModulePath b)

moduleName (MP[])     = "Main"
moduleName (MP (m:_)) = m

moduleDefs ∷ Text → Src.Pkg → [(RepoPath,ModulePath)] → Src.Graph
moduleDefs repo pkg = flip Src.Graph [] . fmap cvt
    where cvt (filename,mp) = Src.Def
            { Src.defPath     = Src.PModule pkg mp
            , Src.defTreePath = Src.PModule pkg mp
            , Src.defName     = moduleName mp
            , Src.defKind     = Src.Module
            , Src.defFile     = filename
            , Src.defDefStart = 0
            , Src.defDefEnd   = 0
            , Src.defExported = True
            , Src.defTest     = False
            , Src.defRepo     = repo
            }

-- We generate a lot of temporary directories:
--   - We copy the root directory of a source unit to keep cabal from
--     writting data to the source directory.
--   - We use a new cabal sandbox per build.
--   - We use tell Haddock to use a separate build directory. (This is
--     probably not necessary).
--   - The graphing process generates a `symbol-graph` file.
graph ∷ C.CabalInfo → IO (Src.Graph, IO ())
graph info = do
  pid ← toInteger <$> getProcessID
  repos ← repoMap info
  modules ∷ Map Loc.ModulePath Src.Pkg ← C.moduleMap info

  let lookupRepo ∷ PkgName → RepoURI
      lookupRepo = fromMaybe "" . flip M.lookup repos
      mkParam k v = "--" <> k <> "=" <> v <> ""
      mkTmp ∷ Text → Text
      mkTmp n = "/tmp/srclib-haskell-" <> n <> "." <> fromString(show pid)
      symbolGraph = mkTmp "symbol-graph"
      sandbox = mkTmp "sandbox"
      buildDir = mkTmp "build-directory"
      workDir = mkTmp "work-directory"
      subDir = srclibPath $ C.cabalPkgDir info
      workSubDir = workDir <> "/" <> subDir
      cleanup = shelly $ do
        let tmps = [symbolGraph, sandbox, buildDir, workDir]
            tmpFilePaths ∷ [Path.FilePath]
            tmpFilePaths = fromText <$> tmps
        mapM_ rm_rf tmpFilePaths

  let toStderr = log_stdout_with $ T.unpack ⋙ hPutStrLn stderr
  let cabal_ = run_ "cabal"

  let packageName = C.cabalPkgName info
  pdb ← mkDB info

  let _4 (_,_,_,x) = x

  let completeSymGraph = mempty∷Src.Graph

  let repo = C.cabalRepoURI info
  let haddockResults = fudgeGraph $ convertGraph repo lookupRepo packageName pdb completeSymGraph

  modRefs ← forM (pdbSourceFileNames pdb) $ \fn → do
    source ← readFile $ fpFromText fn
    return (fn, Imp.moduleRefs (T.unpack fn) source)

  let _3 (a,b,c) = c
      toOffsets ∷ (String,Int,Int) → Int
      toOffsets (fn,l,c) = fromMaybe 0 $ join $ flip Loc.lineColOffset (Loc.LineCol l c) <$> shape
        where frm = Repo $ fromMaybe mempty $ Loc.parseRelativePath $ T.pack fn
              shapes = Set.toList $ fst pdb
              shape = _3 <$> L.find (\(rp,sp,_) → frm ≡ Loc.srcToRepo rp sp) shapes
      toRepoAndPkg ∷ ModulePath → (Src.URI,Src.Pkg)
      toRepoAndPkg mp = trace "toRepoAndPkg" $ traceShow mp $ traceShowId (repo,pkg)
        where repo = fromMaybe "" $ M.lookup pkg repos
              pkg = fromMaybe packageName $ M.lookup mp modules

  let moduleGraph = convertModuleGraph toRepoAndPkg toOffsets modRefs
  let results = moduleGraph ++ haddockResults ++ moduleDefs repo packageName (ourModules pdb)

  -- We can't cleanup here, since we're using lazy IO. Processing the graph file
  -- hasn't (necessarily) happened yet.

  return (results,cleanup)

isParent ∷ RepoPath → RepoPath → Bool
isParent (Repo(FP parent)) (Repo(FP child)) = parent `isSuffixOf` child

-- TODO I can avoid the fromJust by pattern matching on the result of
--      stripPrefix instead of having a separate isParent and stripIt.

stripIt ∷ RepoPath → RepoPath → SrcPath
stripIt (Repo(FP parent)) (Repo(FP child)) =
  Src $ FP $ reverse $ May.fromJust $ stripPrefix (reverse parent) (reverse child)

fe ∷ C.CabalInfo → [(RepoPath, SrcPath, IO FileShape)]
fe info = do
  srcDir ← Set.toList $ C.cabalSrcDirs info
  repoPath ← Set.toList $ C.cabalSrcFiles info
  let srcPath = stripIt srcDir repoPath
  guard $ isParent srcDir repoPath
  return (srcDir, srcPath, fileShape(srclibPath repoPath))

mkDB ∷ C.CabalInfo → IO PathDB
mkDB info = do
  let ugg (srcDir, srcPath, action) = do result ← action
                                         return (srcDir, srcPath, result)
  cols ← Set.fromList <$> mapM ugg (fe info)
  return (cols,M.fromList [])
