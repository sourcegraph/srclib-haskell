{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE NoImplicitPrelude    #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Haddock where

import ClassyPrelude hiding ((</>), (<.>), maximumBy)

import Control.Category
import Control.Category.Unicode
import qualified Data.Maybe as M
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Foldable (maximumBy)

import qualified Documentation.Haddock as H
import Shelly hiding (FilePath, path, (</>), (<.>), canonicalize)
import qualified System.FilePath.Find as P
import System.Posix.Process (getProcessID)

import qualified Cabal as C
import           Locations as Loc
import qualified Srclib as Src

data DefKind = Module | Value | Type
data Def = Def ModulePath Text DefKind Span
data Graph = Graph [Def]

bogusSpan ∷ RepoPath → Span
bogusSpan t = Span t 0 0 -- TODO Maybe Debug.Trace these occurences?

allLocalBindings ∷ H.SymGraph → Set H.LocalBinding
allLocalBindings gr = Set.fromList $ exports ++ internal
  where exports = H.sgExports gr
        internal = lefts $ snd <$> H.sgReferences gr

tmpFileLooksLike ∷ AbsPath → SrcPath → Bool
tmpFileLooksLike (Abs(FP ap)) (Src(FP sp)) = isPrefixOf ap sp

fudgeTmpFile ∷ PathDB → AbsPath → (RepoPath, SrcPath, FileShape)
fudgeTmpFile db path = choose $ Set.filter (srcPath ⋙ tmpFileLooksLike path) db
  where
    srcPath (_,s,_) = s
    choose = maximumBy longest
    longest (_,Src(FP fp1),_) (_,Src(FP fp2),_) = (compare `on` length) fp1 fp2

tmpFileSpan ∷ PathDB → H.FileLoc → Span
tmpFileSpan db (H.FileLoc fnS (l,c) (λ,ξ)) =
  fromMaybe (bogusSpan rp) $ mkSpan rp shape (LineCol l c) (LineCol λ ξ)
    where tmpFn = fromMaybe (error "bad abs path") $ parseAbsPath $ T.pack fnS
          (r,s,shape) = fudgeTmpFile db tmpFn
          rp = srcToRepo r s

localSpan ∷ PathDB → H.LocalBinding → Span
localSpan db (H.Local _ _ loc) = tmpFileSpan db loc

localModule ∷ PathDB → H.LocalBinding → ModulePath
localModule db (H.Local _ _ (H.FileLoc fnS _ _)) = toMod $ fudgeTmpFile db tmpFn
  where tmpFn = fromMaybe (error "bad abs path") $ parseAbsPath $ T.pack fnS
        toMod (_,s,_) = fileToModulePath s

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

convertRef ∷ PathDB → (H.FileLoc,Either H.LocalBinding H.GlobalBinding) → Src.Ref
convertRef db (loc,bind) =
  Src.Ref repoURI "HaskellPackage" defUnit defPath True file start end
    where repoURI = "" -- TODO
          defUnit = "" -- TODO
          defPath = "" -- TODO
          (Span file start width) = tmpFileSpan db loc
          end = start + width

convertGraph ∷ Text → PathDB → H.SymGraph → Src.Graph
convertGraph pkg db gr = Src.Graph defs refs
  where defs = convertDef pkg db <$> Set.toList(allLocalBindings gr)
        refs = convertRef db <$> H.sgReferences gr

{-

data Binding = Binding
  { bindModule ∷ ModulePath
  , bindName   ∷ Text
  , bindKind   ∷ DefKind
  , bindSpan   ∷ Span
  }

deriving instance Show DefKind
deriving instance Show Def
deriving instance Show Graph

defsFromHaddock ∷ C.CabalInfo → H.InstalledInterface → IO [Def]
defsFromHaddock info iface = do
  exportedDefs' ← mapM (nameDef info) $ H.instExports iface
  let exportedDefs = catMaybes exportedDefs'
  modDef ← moduleDef info iface
  return $ modDef : exportedDefs

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

graph ∷ C.CabalInfo → IO Graph
graph info = do
  pid ← toInteger <$> getProcessID

  let mkParam k v = "--" ++ k ++ "=" ++ v ++ "" -- Escape v!
      mkTmp n = "/tmp/srclib-haskell-" <> n <> "." <> fromString(show pid)
      symbolGraph = mkTmp "symbol-graph"
      sandbox = mkTmp "sandbox"
      buildDir = mkTmp "build-directory"

  let toStderr = log_stdout_with $ T.unpack >>> hPutStrLn stderr
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

  ifaceFile ← either error id <$>
    H.readInterfaceFile H.freshNameCache (T.unpack symbolGraph)

  let ifaces = H.ifInstalledIfaces ifaceFile
  haddockDefs ← mapM (defsFromHaddock info) ifaces
  return $ Graph $ concat haddockDefs
-}
