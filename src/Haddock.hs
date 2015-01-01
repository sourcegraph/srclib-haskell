{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE NoImplicitPrelude    #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Haddock where

import ClassyPrelude hiding ((</>), (<.>))
import Control.Category
import qualified Data.Maybe as M
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Documentation.Haddock as Haddock
import GHC
import Name
import Shelly hiding (FilePath, path, (</>), (<.>), canonicalize)
import qualified System.FilePath.Find as P
import System.Path as P
import System.Posix.Process (getProcessID)

import qualified Cabal as C
import qualified Locations as Loc

data DefKind = Module | Value | Type
data Def = Def Loc.ModulePath Text DefKind Loc.Span
data Graph = Graph [Def]

data Binding = Binding
  { bindModule ∷ Loc.ModulePath
  , bindName   ∷ Text
  , bindKind   ∷ DefKind
  , bindSpan   ∷ Loc.Span
  }

deriving instance Show DefKind
deriving instance Show Def
deriving instance Show Graph

mkParam :: ∀m.(Monoid m,IsString m) ⇒ m → m → m
mkParam k v = "--" ++ k ++ "=" ++ v ++ "" -- Escape v!

defsFromHaddock ∷ C.CabalInfo → Haddock.InstalledInterface → IO [Def]
defsFromHaddock info iface = do
  exportedDefs' ← mapM (nameDef info) $ Haddock.instExports iface
  let exportedDefs = catMaybes exportedDefs'
  modDef ← moduleDef info iface
  return $ modDef : exportedDefs

findModuleFile = undefined

srcSpanSpan = undefined

moduleDef ∷ C.CabalInfo → Haddock.InstalledInterface → IO Def
moduleDef info iface = do
  let modNm = T.pack $ moduleNameString $ moduleName $ Haddock.instMod iface
      modPath = M.fromJust $ Loc.parseModulePath modNm -- TODO
  fnMay ← findModuleFile (Set.toList $ C.cabalSrcDirs info) modPath
  let fn = show $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  return $ Def modPath modNm Module $ Loc.Span (T.pack fn) 0 0

nameDef ∷ C.CabalInfo → Name → IO(Maybe Def)
nameDef info nm = do
  let modul = nameModule nm
      srcSpan = nameSrcSpan nm
      modName = T.pack $ moduleNameString $ moduleName modul
      nameStr = T.pack $ occNameString $ getOccName nm

  fnMay ← findModuleFile (Set.toList $ C.cabalSrcDirs info) $ M.fromJust $ Loc.parseModulePath modName
  let fn = tshow $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  let ugg (a,b,c) = Loc.Span a b c
  loc ← ugg <$> fromMaybe (fn,0,0) <$> srcSpanSpan fn srcSpan
  return $ Just $ Def (Loc.MP nameStr $ T.splitOn "." modName) nameStr Value loc

graph ∷ C.CabalInfo → IO Graph
graph info = do
  pid ← toInteger <$> getProcessID

  let mktemp n = "/tmp/srclib-haskell-" <> n <> "." <> fromString(show pid)
      symbolGraph = mktemp "symbol-graph"
      sandbox = mktemp "sandbox"
      buildDir = mktemp "build-directory"

  let toStderr = log_stdout_with $ (T.unpack >>> hPutStrLn stderr)
  let cabal_ = run_ "cabal"

  shelly $ toStderr $ do
    haddockPath ← fromMaybe (error "srclib-haddock is not installed!") <$>
      which "srclib-haddock"

    cd $ fromText $ fromString $ P.getPathString $ P.dropFileName $ C.cabalFile info
    cabal_ ["sandbox", "init", mkParam "sandbox" sandbox]
    cabal_ ["install", "--only-dependencies"]
    cabal_ ["configure", mkParam "builddir" buildDir]
    cabal_ [ "haddock", "--executables", "--internal"
           , mkParam "with-haddock" $ toTextIgnore haddockPath
           , mkParam "haddock-options" ("-D" <> symbolGraph)
           , mkParam "builddir" buildDir
           ]

  ifaceFile ← either error id <$>
    Haddock.readInterfaceFile Haddock.freshNameCache (T.unpack symbolGraph)

  let ifaces = Haddock.ifInstalledIfaces ifaceFile
  haddockDefs ← mapM (defsFromHaddock info) ifaces
  return $ Graph $ concat haddockDefs
