{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE NoImplicitPrelude    #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Haddock where

import           ClassyPrelude hiding ((</>), (<.>), maximumBy)
import qualified Prelude
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

import           Text.Printf (printf)

import qualified Documentation.Haddock as H
import           Shelly hiding (FilePath, path, (</>), (<.>), canonicalize, trace)
import qualified Shelly
import qualified Filesystem.Path.CurrentOS as Path
import           System.Posix.Process (getProcessID)
import           Text.Regex.TDFA

import qualified Cabal as C
import           Locations as Loc
import qualified Srclib as Src

import Distribution.Hackage.DB (Hackage, readHackage)

data Def = Def ModulePath Text Src.Kind Span
data Bind = Bind String H.NameSpc H.FileLoc Bool Text
  deriving (Show,Ord,Eq)

localUniq (H.Local _ u _ _) = u

mkIndex ∷ Ord k ⇒ (a → k) → [a] → Map k a
mkIndex f = map (\r→(f r,r)) ⋙ M.fromList

composeN ∷ Int → (a → a) → (a → a)
composeN 0 _ = id
composeN 1 f = f
composeN n f = composeN (n-1) (f.f)

allTheRenames ∷ H.SymGraph → H.SymGraph
allTheRenames = composeN 10 renames

instance Monoid H.SymGraph where
 mempty = H.SymGraph [] [] []
 mappend (H.SymGraph a b c) (H.SymGraph d e f) = H.SymGraph (a++d) (b++e) (c++f)

renames ∷ H.SymGraph → H.SymGraph
renames (H.SymGraph refs exports renames) =
  H.SymGraph (rename <$> refs) (rnm <$> exports) renames
    where rnmTbl = M.fromList renames
          defsByUniq = mkIndex localUniq $ lefts $ snd <$> refs
          rename (fl,Left(l)) = (fl, Left(rnm l))
          rename x            = x
          rnm def@(H.Local nm u nmSpc floc) = fromMaybe def $ do
            otherUniq ← M.lookup u rnmTbl
            realDef ← M.lookup otherUniq defsByUniq
            return realDef

allLocalBindings ∷ H.SymGraph → Set Bind
allLocalBindings gr = Set.fromList internal
  where exports = H.sgExports gr
        internal = mkBind <$> (lefts $ snd <$> H.sgReferences gr)
        isExported l = not $ L.null $ flip L.filter (H.sgExports gr) $ theSame l
        theSame (H.Local _ uniq1 _ _) (H.Local _ uniq2 _ _) = uniq1 ≡ uniq2
        mkBind ∷ H.LocalBinding → Bind
        mkBind l@(H.Local nm u nmSpc floc) =
          Bind nm nmSpc floc (isExported l) (T.pack u)

sgReferences ∷ H.SymGraph → [(H.FileLoc,(Either Bind H.GlobalBinding))]
sgReferences gr = map f $ H.sgReferences gr
  where f (loc,Left l) = (loc,Left $ mkBind l)
        f (loc,Right r) = (loc,Right r)
        isExported l = not $ L.null $ flip L.filter (H.sgExports gr) $ theSame l
        theSame (H.Local _ uniq1 _ _) (H.Local _ uniq2 _ _) = uniq1 ≡ uniq2
        mkBind ∷ H.LocalBinding → Bind
        mkBind l@(H.Local nm u nmSpc floc) =
          Bind nm nmSpc floc (isExported l) (T.pack u)

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

tmpFileSpan ∷ PathDB → H.FileLoc → Span
tmpFileSpan pdb@(db,tmps) (H.FileLoc fnS (l,c) (λ,ξ)) =
  fromMaybe bogusSpan $ mkSpan rp shape (LineCol l c) (LineCol λ ξ)
    where (r,s,shape) = fudgeTmpFile (db,tmps) fnS
          m = tmpFileModule pdb fnS
          rp = srcToRepo r s

localSpan ∷ PathDB → Bind → Span
localSpan db (Bind _ _ loc e _) = tmpFileSpan db loc

bogusModule ∷ PathDB → H.LocalBinding → ModulePath
bogusModule (_,tmps) (H.Local fnS _ _ _) = trace warning $ parseModulePath $ T.pack fnS
  where warning = "SymGraph is inconsistent. This file does not occur: " <> fnS
               <> "\n" <> show tmps

localModule ∷ PathDB → Bind → ModulePath
localModule db@(_,tmps) loc@(Bind _ _ (H.FileLoc fnS _ _) _ _) =
  tmpFileModule db fnS

isVersionNumber ∷ Text → Bool
isVersionNumber = T.unpack ⋙ (=~ ("^([0-9]+\\.)*[0-9]+$"::String))

fudgePkgName ∷ Text → Text
fudgePkgName pkg =
  T.intercalate "-" $ reverse $ case reverse $ T.split (≡'-') pkg of
    []                 → []
    full@(last:before) → if isVersionNumber last then before else full

makeSrclibPath ∷ H.NameSpc → Text → ModulePath → Text → Maybe Text → Src.Path
makeSrclibPath kind pkg mp nm uniq = case uniq of
  Nothing -> Src.PGlobal pkg mp nm (convertKind kind)
  Just u  -> Src.PLocal pkg mp nm (convertKind kind) u

convertKind ∷ H.NameSpc → Src.Kind
convertKind H.Value = Src.Value
convertKind H.Type = Src.Type

locStartLC ∷ H.FileLoc → LineCol
locStartLC (H.FileLoc _ (l,c) _) = LineCol l c

convertDef ∷ Text → PathDB → Bind → Src.Def
convertDef pkg db l@(Bind nm nmSpc loc exported u) =
  Src.Def spath spath (T.pack nm) kind file start end exported False
    where (Span file start width) = localSpan db l
          end = start+width
          kind = convertKind nmSpc
          modul = localModule db l
          spath = makeSrclibPath nmSpc pkg modul (T.pack nm) pos
          pos = if exported then Nothing else Just u

globalPath ∷ H.GlobalBinding → Src.Path
globalPath glob@(H.Global nm nmSpc modul pkg) =
  makeSrclibPath nmSpc (fudgePkgName $ T.pack pkg) mp (T.pack nm) Nothing
    where mp = (parseModulePath $ T.pack modul)

baseRepo = "github.com/bsummer4/packages-base"

safeHead ∷ [a] → Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

convertRef ∷ (Text→Text) → Text → PathDB → (H.FileLoc,Either Bind H.GlobalBinding) → Src.Ref
convertRef lookupRepo pkg db (loc,bind) =
  Src.Ref repoURI "HaskellPackage" defUnit defPath isDef file start end
    where repoURI = lookupRepo defUnit
          defUnit = Src.pathPkg defPath
          startsAt (H.FileLoc _ s1 _) (H.FileLoc _ s2 _) = s1≡s2
          isDef = case bind of Right _ → False
                               Left (Bind _ _ bloc _ _) → loc `startsAt` bloc

          defPath = either (Src.defPath ⋘ convertDef pkg db) globalPath bind
          (Span file start width) = tmpFileSpan db loc
          end = start + width

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

convertGraph ∷ (Text→Text) → Text → PathDB → H.SymGraph → Src.Graph
convertGraph lookupRepo pkgWithJunk db agr = fudgeGraph $ Src.Graph defs refs
  where pkg = traceShowId $ fudgePkgName $ traceShowId pkgWithJunk
        defs = convertDef pkg db <$> Set.toList(allLocalBindings gr)
        refs = convertRef lookupRepo pkg db <$> sgReferences gr
        gr = allTheRenames agr

-- TODO Using `read` directly is not safe.
-- TODO Read/Show is a terrible approach to serializing to disk!
readSymGraphFile ∷ String → H.SymGraphFile
readSymGraphFile = fmap Prelude.read . lines

loadSymGraphFile ∷ Path.FilePath → IO H.SymGraphFile
loadSymGraphFile = readFile >=> (readSymGraphFile ⋙ return)

tmpFiles ∷ H.SymGraphFile → Map String ModulePath
tmpFiles = M.fromList . map (\(f,m,_,_)→(f,parseModulePath$T.pack m))

instance Semigroup Shelly.FilePath where
  a <> b = a `mappend` b

type PkgName = Text
type RepoURI = Text

repoMap ∷ C.CabalInfo → IO (Map PkgName RepoURI)
repoMap info = do
  hack ← readHackage
  ds ← mapM(C.resolve hack ⋙ return) $ M.toList $ C.cabalDependencies info
  return $ M.fromList $ (\d → (Src.depToUnit d, Src.depToRepoCloneURL d)) <$> ds

type ModuleLookup = ModulePath → (Src.URI,Src.Pkg)
type SrcLocLookup = (String,Int,Int) → Int
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

moduleDefs ∷ Src.Pkg → [(RepoPath,ModulePath)] → Src.Graph
moduleDefs pkg = flip Src.Graph [] . fmap cvt
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

  shelly $ toStderr $ do
    mkdir_p (fromText workDir)
    let wd = T.unpack workDir
        tarcmd = T.pack $ printf "(tar c *) | (cd '%s'; tar x)" wd
    run_ "/bin/sh" ["-c", tarcmd]

    cd (fromText workDir)
    errExit False $ run_ "autoreconf" []

    cd (fromText workSubDir)
    errExit False $ run_ "autoreconf" []

    cabal_ ["sandbox", "init", mkParam "sandbox" sandbox]
    errExit False $
      cabal_ [ "install", "--only-dependencies"
                        , "-j4"
                        , "--disable-optimization"
                        , "--force-reinstalls"
                        ]

    cabal_ ["configure", mkParam "builddir" buildDir]
    cabal_ [ "haddock", "--executables", "--internal"
           , mkParam "haddock-options" ("-G" <> symbolGraph)
           , mkParam "builddir" buildDir
           ]

  let badLoad = error $ T.unpack $ "Unable to load file: " <> symbolGraph
  graphs ← loadSymGraphFile $ Path.decodeString $ T.unpack symbolGraph

  let packageName = C.cabalPkgName info
  pdb ← mkDB info graphs

  let _4 (_,_,_,x) = x

  let completeSymGraph = mconcat $ _4 <$> graphs

  let haddockResults = fudgeGraph $ convertGraph lookupRepo packageName pdb completeSymGraph

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
  let results = moduleGraph ++ haddockResults ++ moduleDefs packageName (ourModules pdb)

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

mkDB ∷ C.CabalInfo → H.SymGraphFile → IO PathDB
mkDB info graphFile = do
  let ugg (srcDir, srcPath, action) = do result ← action
                                         return (srcDir, srcPath, result)
  cols ← Set.fromList <$> mapM ugg (fe info)
  return (cols,tmpFiles graphFile)
