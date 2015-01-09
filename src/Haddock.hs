{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE NoImplicitPrelude    #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Haddock where

import ClassyPrelude hiding ((</>), (<.>), maximumBy)

import qualified Prelude

import System.Posix.Directory

import qualified Data.IntMap as IntMap
import Control.Category.Unicode
import qualified Data.Maybe as May
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Foldable (maximumBy)

import qualified Documentation.Haddock as H
import Shelly hiding (FilePath, path, (</>), (<.>), canonicalize, trace)
import qualified Filesystem.Path.CurrentOS as Path
import System.Posix.Process (getProcessID)

import qualified Cabal as C
import           Locations as Loc
import qualified Srclib as Src

data DefKind = Module | Value | Type
data Def = Def ModulePath Text DefKind Span

allLocalBindings ∷ H.SymGraph → Set H.LocalBinding
allLocalBindings gr = Set.fromList $ exports ++ internal
  where exports = H.sgExports gr
        internal = lefts $ snd <$> H.sgReferences gr

_3 (a,b,c) = c

fudgeTmpFile ∷ PathDB → String → (RepoPath, SrcPath, FileShape)
fudgeTmpFile (db,tmps) tmp = chooseFrom matches
  where modul = M.lookup tmp tmps
        srcPath (_,s,_) = s
        q m = Set.filter (srcPath ⋙ srcPathMatch m) db
        matches = fromMaybe Set.empty $ q <$> modul
        longest (_,Src(FP f),_) (_,Src(FP φ),_) = (compare `on` length) f φ
        fallback = (Repo(FP([])), Src(FP([])), Shape [] IntMap.empty)
        chooseFrom s = if Set.null s then fallback else maximumBy longest s

bogusSpan ∷ RepoPath → Span
bogusSpan t = Span t 0 0 -- TODO Maybe Debug.Trace these occurences?

tmpFileSpan ∷ PathDB → H.FileLoc → Span
tmpFileSpan (db,tmps) (H.FileLoc fnS (l,c) (λ,ξ)) =
  fromMaybe (bogusSpan rp) $ mkSpan rp shape (LineCol l c) (LineCol λ ξ)
    where (r,s,shape) = fudgeTmpFile (db,tmps) fnS
          m = M.lookup fnS tmps
          rp = srcToRepo r s

localSpan ∷ PathDB → H.LocalBinding → Span
localSpan db (H.Local _ _ loc) = tmpFileSpan db loc

localModule ∷ PathDB → H.LocalBinding → ModulePath
localModule (_,tmps) (H.Local _ _ (H.FileLoc fnS _ _)) =
  fromMaybe (error("bad path: " <> fnS)) $ M.lookup fnS tmps

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

-- This drops defs with duplicated paths.
fudge ∷ Src.Graph → Src.Graph
fudge (Src.Graph defs refs) = Src.Graph (fudgeDefs defs) (fudgeRefs refs)

fudgeBy ∷ Ord b ⇒ (a → b) → [a] → [a]
fudgeBy f = M.elems . M.fromList . map (\x→(f x,x))

fudgeDefs ∷ [Src.Def] → [Src.Def]
fudgeDefs = fudgeBy Src.defPath

fudgeRefs ∷ [Src.Ref] → [Src.Ref]
fudgeRefs = fudgeBy $ \r → (Src.refStart r, Src.refEnd r)

pos (x,y,nm) = (T.pack $ show x) <> ":" <> (T.pack $ show y) <> " (" <> nm

summary ∷ Src.Graph → Text
summary (Src.Graph dfs rfs) =
  unlines("[refs]" : (pos<$>refs)) <> unlines("[defs]" : (pos<$>defs))
    where defs = (\x→(Src.defDefStart x, Src.defDefEnd x, Src.defPath x)) <$> dfs
          refs = (\x→(Src.refStart x, Src.refEnd x, Src.refDefPath x)) <$> rfs

convertGraph ∷ Text → PathDB → H.SymGraph → Src.Graph
convertGraph pkg db gr = fudge $ Src.Graph defs refs
  where defs = convertDef pkg db <$> Set.toList(allLocalBindings gr)
        refs = convertRef pkg db <$> H.sgReferences gr

readSymGraphFile ∷ String → SymGraphFile
readSymGraphFile = fmap Prelude.read . lines -- TODO Using `read` directly is not safe.

type SymGraphFile = [(String, H.ModuleNm, H.Pkg, H.SymGraph)]

loadSymGraphFile ∷ Path.FilePath → IO(SymGraphFile)
loadSymGraphFile = readFile >=> (readSymGraphFile ⋙ return)

tmpFiles ∷ SymGraphFile → Map String ModulePath
tmpFiles = M.fromList . map (\(f,m,_,_)→(f,parseModulePath$T.pack m))

graph ∷ C.CabalInfo → IO Src.Graph
graph info = do
  pid ← toInteger <$> getProcessID

  let mkParam k v = "--" <> k <> "=" <> v <> ""
      mkTmp n = "/tmp/srclib-haskell-" <> n <> "." <> fromString(show pid)
      symbolGraph = mkTmp "symbol-graph"
      sandbox = mkTmp "sandbox"
      buildDir = mkTmp "build-directory"

  let toStderr = log_stdout_with $ T.unpack ⋙ hPutStrLn stderr
  let cabal_ = run_ "cabal"

  shelly $ toStderr $ do
    cd $ fromText $ srclibPath $ C.cabalPkgDir info
    cabal_ ["sandbox", "init", mkParam "sandbox" sandbox]
    cabal_ ["install", "--only-dependencies"]
    cabal_ ["configure", mkParam "builddir" buildDir]
    cabal_ [ "haddock", "--executables", "--internal"
           , mkParam "haddock-options" ("-G" <> symbolGraph)
           , mkParam "builddir" buildDir
           ]

  let badLoad = error $ T.unpack $ "Unable to load file: " <> symbolGraph
  graphs ← loadSymGraphFile $ Path.decodeString $ T.unpack symbolGraph

  traceM "<graphs>"
  traceShowM graphs
  traceM "</graphs>"

  let pkg = C.cabalPkgName info
  pdb ← mkDB info graphs

  return $ fudge $ mconcat $ (_4 ⋙ convertGraph pkg pdb) <$> graphs

_4 (_,_,_,x) = x

isParent ∷ RepoPath → RepoPath → Bool
isParent (Repo(FP parent)) (Repo(FP child)) = isSuffixOf parent child

-- TODO I can avoid the fromJust by pattern matching on the result of
--      stripPrefix instead of having a separate isParent and stripIt.

stripIt ∷ RepoPath → RepoPath → SrcPath
stripIt (Repo(FP parent)) (Repo(FP child)) =
  Src $ FP $ reverse $ May.fromJust $ stripPrefix (reverse parent) (reverse child)

fe2 info = [ (srcDir, stripIt srcDir repoPath, fileShape(srclibPath repoPath))
               | srcDir   ← Set.toList $ C.cabalSrcDirs info
               , repoPath ← Set.toList $ C.cabalSrcFiles info
               , isParent srcDir repoPath
               ]

fe info = do
  srcDir ← Set.toList $ C.cabalSrcDirs info
  repoPath ← Set.toList $ C.cabalSrcFiles info
  let srcPath = stripIt srcDir repoPath
  guard $ isParent srcDir repoPath
  return (srcDir, srcPath, fileShape(srclibPath repoPath))

mkDB ∷ C.CabalInfo → SymGraphFile → IO PathDB
mkDB info sgr = do
  let ugg (srcDir, srcPath, action) = do result ← action
                                         return (srcDir, srcPath, result)
  cols ← Set.fromList <$> mapM ugg (fe info)
  return (cols,tmpFiles sgr)

finfO = C.CabalInfo { C.cabalFile = Repo (FP ["haskell-hello-world.cabal"])
                    , C.cabalPkgName = "haskell-hello-world"
                    , C.cabalPkgDir = Repo (FP [])
                    , C.cabalDependencies = Set.fromList ["base"]
                    , C.cabalSrcFiles = Set.fromList [ Repo (FP ["Main.hs","src"])
                                                     , Repo (FP ["Setup.hs"])
                                                     ]
                    , C.cabalSrcDirs = Set.fromList [Repo (FP ["src"])]
                    , C.cabalGlobs = Set.fromList ["src/**/*.hs"]
                    }

finfo = C.CabalInfo { C.cabalFile = Repo (FP ["deepseq.cabal"])
                    , C.cabalPkgName = "deepseq"
                    , C.cabalPkgDir = Repo (FP [])
                    , C.cabalDependencies = Set.fromList ["HUnit","array","base","ghc-prim","test-framework","test-framework-hunit"]
                    , C.cabalSrcFiles = Set.fromList [ Repo (FP ["DeepSeq.hs","Control"])
                                                     , Repo (FP ["Main.hs","tests"])
                                                     , Repo (FP ["Setup.hs"])
                                                     ]
                    , C.cabalSrcDirs = Set.fromList [Repo (FP [])]
                    , C.cabalGlobs = Set.fromList ["./**/*.hs"]
                    }

htest = do
  changeWorkingDirectory "../testdata/case/deepseq"
  gr ← loadSymGraphFile "/tmp/srclib-haskell-symbol-graph.11461"
  print "<GR>"
  print gr
  print "</GR>"
  pdb ← mkDB finfo gr
  traceM "<PDB>"
  traceShowM pdb
  traceM "</PDB>"

  -- let f pdb (_,_,pkg,gr) = convertGraph (T.pack pkg) pdb gr
  -- mapM_ print $ f pdb <$> gr
  -- print "=================="
  -- mapM_ print $ f pdb <$> gr
  -- print "=================="

  let result = mconcat $ (_4 ⋙ convertGraph "deepseq" pdb) <$> gr
  traceM "<result>"
  traceM $ T.unpack $ summary result
  traceM "</result>"

{-

findModuleFile = undefined

srcSpanSpan = undefined

moduleDef ∷ C.CabalInfo → H.InstalledInterface → IO Def
moduleDef info iface = do
  let modNm = T.pack $ moduleNameString $ moduleName $ H.instMod iface
      modPath = parseModulePath modNm -- TODO
  fnMay ← findModuleFile (Set.toList $ C.cabalSrcDirs info) modPath
  let fn = show $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  return $ Def modPath modNm Module $ Span (T.pack fn) 0 0

nameDef ∷ C.CabalInfo → Name → IO(Maybe Def)
nameDef info nm = do
  let modul = nameModule nm
      srcSpan = nameSrcSpan nm
      modName = T.pack $ moduleNameString $ moduleName modul
      nameStr = T.pack $ occNameString $ getOccName nm

  fnMay ← findModuleFile (Set.toList $ C.cabalSrcDirs info) $ parseModulePath modName
  let fn = tshow $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  let ugg (a,b,c) = Span a b c
  loc ← ugg <$> fromMaybe (fn,0,0) <$> srcSpanSpan fn srcSpan
  let mod = MP $ nameStr : T.splitOn "." modName
  return $ Just $ Def mod nameStr Value loc

-}
