{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE OverloadedLists      #-}
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

import qualified Cabal as C
import           Locations as Loc
import qualified Srclib as Src

data DefKind = Module | Value | Type
data Def = Def ModulePath Text DefKind Span

allLocalBindings ∷ H.SymGraph → Set H.LocalBinding
allLocalBindings gr = Set.fromList $ exports ++ internal
  where exports = H.sgExports gr
        internal = lefts $ snd <$> H.sgReferences gr

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

tmpFileModule ∷ PathDB → String → ModulePath
tmpFileModule db tmp = fileToModulePath $ srcPath $ fudgeTmpFile db tmp

tmpFileSpan ∷ PathDB → H.FileLoc → Span
tmpFileSpan pdb@(db,tmps) (H.FileLoc fnS (l,c) (λ,ξ)) =
  fromMaybe bogusSpan $ mkSpan rp shape (LineCol l c) (LineCol λ ξ)
    where (r,s,shape) = fudgeTmpFile (db,tmps) fnS
          m = tmpFileModule pdb fnS
          rp = srcToRepo r s

localSpan ∷ PathDB → H.LocalBinding → Span
localSpan db (H.Local _ _ loc) = tmpFileSpan db loc

bogusModule ∷ PathDB → H.LocalBinding → ModulePath
bogusModule (_,tmps) (H.Local fnS _ _) = trace warning $ parseModulePath $ T.pack fnS
  where warning = "SymGraph is inconsistent. This file does not occur: " <> fnS
               <> "\n" <> show tmps

localModule ∷ PathDB → H.LocalBinding → ModulePath
localModule db@(_,tmps) loc@(H.Local _ _ (H.FileLoc fnS _ _)) =
  tmpFileModule db fnS

makeSrclibPath ∷ H.NameSpc → Text → ModulePath → Text → Text
makeSrclibPath kind pkg (MP mp) nm =
  T.intercalate "/" $ [pkg] ++ mp ++ [nm,convertKind kind]

convertKind ∷ H.NameSpc → Text
convertKind H.Value = "Value"
convertKind H.Type = "Type"

convertDef ∷ Text → PathDB → H.LocalBinding → Src.Def
convertDef pkg db loc@(H.Local nm nmSpc _) =
  Src.Def spath spath (T.pack nm) kind file start end True False
    where (Span file start width) = localSpan db loc
          end = start+width
          kind = convertKind nmSpc
          modul = localModule db loc
          spath = makeSrclibPath nmSpc pkg modul (T.pack nm)

globalPath ∷ H.GlobalBinding → Text
globalPath glob@(H.Global nm nmSpc modul pkg) =
  makeSrclibPath nmSpc (T.pack pkg) (parseModulePath $ T.pack modul) (T.pack nm)

convertRef ∷ Text → PathDB → (H.FileLoc,Either H.LocalBinding H.GlobalBinding) → Src.Ref
convertRef pkg db (loc,bind) =
  Src.Ref repoURI "HaskellPackage" defUnit defPath True file start end
    where repoURI = ""
          defUnit = ""
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
pos (x,y,nm) = T.pack(show x) <> ":" <> T.pack(show y) <> " (" <> nm

summary ∷ Src.Graph → Text
summary (Src.Graph dfs rfs) =
  unlines("[refs]" : (pos<$>refs)) <> unlines("[defs]" : (pos<$>defs))
    where defs = (\x→(Src.defDefStart x, Src.defDefEnd x, Src.defPath x)) <$> dfs
          refs = (\x→(Src.refStart x, Src.refEnd x, Src.refDefPath x)) <$> rfs

convertGraph ∷ Text → PathDB → H.SymGraph → Src.Graph
convertGraph pkg db gr = fudgeGraph $ Src.Graph defs refs
  where defs = convertDef pkg db <$> Set.toList(allLocalBindings gr)
        refs = convertRef pkg db <$> H.sgReferences gr

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

graph ∷ C.CabalInfo → IO Src.Graph
graph info = do
  pid ← toInteger <$> getProcessID

  let mkParam k v = "--" <> k <> "=" <> v <> ""
      mkTmp ∷ Text → Text
      mkTmp n = "/tmp/srclib-haskell-" <> n <> "." <> fromString(show pid)
      symbolGraph = mkTmp "symbol-graph"
      sandbox = mkTmp "sandbox"
      buildDir = mkTmp "build-directory"
      workDir = mkTmp "work-directory"
      fromDir = fromText $ srclibPath $ C.cabalPkgDir info

  let toStderr = log_stdout_with $ T.unpack ⋙ hPutStrLn stderr
  let cabal_ = run_ "cabal"
  let cabal_ = run_ "cabal"

  shelly $ toStderr $ do

    -- Use a temporary working directory to avoid touching the user's files.
    mkdir_p (fromText workDir)
    let tarcmd∷String = printf "(tar c *) | (cd '%s'; tar x)" $ T.unpack workDir
    run_ "/bin/sh" ["-c", T.pack tarcmd]
    cd (fromText workDir)

    cabal_ ["sandbox", "init", mkParam "sandbox" sandbox]
    cabal_ ["install", "--only-dependencies", "-j4", "--disable-optimization"]
    cabal_ ["configure", mkParam "builddir" buildDir]
    cabal_ [ "haddock", "--executables", "--internal"
           , mkParam "haddock-options" ("-G" <> symbolGraph)
           , mkParam "builddir" buildDir
           ]

  let badLoad = error $ T.unpack $ "Unable to load file: " <> symbolGraph
  graphs ← loadSymGraphFile $ Path.decodeString $ T.unpack symbolGraph

  let pkg = C.cabalPkgName info
  pdb ← mkDB info graphs

  let _4 (_,_,_,x) = x
  return $ fudgeGraph $ mconcat $ (_4 ⋙ convertGraph pkg pdb) <$> graphs

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
