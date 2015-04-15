{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Main where

import ClassyPrelude
import Control.Category.Unicode
import Data.Aeson as JSON
import qualified Data.Set as Set
import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T

import qualified Distribution.Version as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.Text as Cabal

import qualified Data.Map as M

import           System.FilePath.Find                          ((==?))
import qualified System.FilePath.Find                          as Find

import Filesystem.Path.CurrentOS as Path

import qualified System.Environment as Sys
import qualified System.Directory as Sys

import Cabal as C
import Haddock as H
import Srclib as Src
import qualified Locations as Loc

import Distribution.Hackage.DB (Hackage, readHackage)


depresolveCmd ∷ CabalInfo → IO [Src.ResolvedDependency]
depresolveCmd info = return []

getCabalInfo ∷ SourceUnit → CabalInfo
getCabalInfo x = case C.fromSrcUnit x of
                   Just a → a
                   Nothing → error "NO CABAL INFO"

usage ∷ IO ()
usage = do
  progName ← T.pack <$> Sys.getProgName
  putStrLn "Usage:"
  putStrLn $ T.concat ["    ", progName, " scan"]
  putStrLn $ T.concat ["    ", progName, " graph < sourceUnit"]
  putStrLn $ T.concat ["    ", progName, " depresolve < sourceUnit"]
  putStrLn progName

withCabalInfoFromStdin ∷ ToJSON a ⇒ (CabalInfo → IO a) → IO ()
withCabalInfoFromStdin proc = do
  unit ← JSON.decode <$> LBS.getContents
  maybe usage (proc >=> dumpJSON) $ getCabalInfo <$> unit

dumpJSON ∷ ToJSON a ⇒ a → IO ()
dumpJSON = JSON.encode ⋙ BC.putStrLn

runGrapher ∷ IO ()
runGrapher = do
  unit ← JSON.decode <$> LBS.getContents
  case getCabalInfo <$> unit of
    Nothing → usage
    Just info → do
      (r,cleanup) ← graph info
      dumpJSON r
      cleanup

srclibRun ∷ [Text] → IO ()
srclibRun ("scan":_) = (map C.toSrcUnit <$> scan) >>= dumpJSON
srclibRun ["depresolve"] = withCabalInfoFromStdin depresolveCmd
srclibRun ["graph"] = runGrapher
srclibRun _ = usage

main ∷ IO ()
main = do
  args ← getArgs
  dir ← Sys.getCurrentDirectory
  -- (show ⋙ (("srclib-haskell @" ++ dir ++ " ")++) ⋙ hPutStrLn stderr) args
  srclibRun args

fromRight ∷ Either Text b → b
fromRight (Left x) = error $ T.unpack x
fromRight (Right y) = y

bindLeft ∷ a → Maybe b → Either a b
bindLeft x Nothing = Left x
bindLeft _ (Just y) = Right y

pToStr ∷ FilePath → String
pFromStr ∷ String → FilePath
(pToStr,pFromStr) = (Path.encodeString, Path.decodeString)

allRepoFiles ∷ FilePath → IO (Map Loc.RepoPath FilePath)
allRepoFiles rootDir = do
  let pathText ∷ FilePath → Either Text Loc.RepoPath
      pathText p = do
        let rootDir' = pFromStr(pToStr rootDir <> "/")
        relPath ← bindLeft "Bad path" (Path.stripPrefix rootDir' p)
        textified ← Path.toText relPath
        let parseError = "Invalid relative path: " <> textified
        parsed ← bindLeft parseError $ Loc.parseRelativePath textified
        return $ Loc.Repo parsed
      toRepoPath = pathText ⋙ fromRight

  let fQuery = Find.fileType ==? Find.RegularFile
  files ← map pFromStr <$> Find.find Find.always fQuery (pToStr rootDir)
  return $ M.fromList $ (\f→(toRepoPath f, f)) <$> files

scan ∷ IO [CabalInfo]
scan = do
  rootDir ← (Path.fromText ⋘ T.pack) <$> Sys.getCurrentDirectory
  fdb ← allRepoFiles rootDir
  cabalFiles ← mapM readFile $ M.filterWithKey (\k _→C.isCabalFile k) fdb
  let repo = C.Repo fdb cabalFiles
  return $ M.elems $ snd $ C.analyse repo

testScan ∷ IO ()
testScan = scan >>= mapM_ print
