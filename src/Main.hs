{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Main where

import ClassyPrelude
import Control.Category
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T

import qualified Shelly

import qualified Data.Map as M

import           System.FilePath.Find                          ((==?))
import qualified System.FilePath.Find                          as Find

import Filesystem.Path.CurrentOS as Path

import qualified System.Environment as Sys
import qualified System.Directory as Sys

import Cabal as C
--port Haddock as H
import Srclib as Src
import qualified Locations as Loc

depresolveCmd ∷ CabalInfo → IO ()
depresolveCmd = error . show

getCabalInfo ∷ SourceUnit → CabalInfo
getCabalInfo = error . show

{-
dirname = undefined

hsGlobs ∷ Text → [Text]
hsGlobs x = undefined

toSourceUnit ∷ CabalInfo → SourceUnit
toSourceUnit (CabalInfo f pkg deps files dirs) =
  SourceUnit f pkg (dirname f) deps dirs files (concatMap hsGlobs dirs)

instance ToJSON H.Graph where
 toJSON = error . show

instance ToJSON CabalInfo where
 toJSON = toJSON . toSourceUnit

dumpJSON ∷ ToJSON a ⇒ a → IO ()
dumpJSON = encode >>> BC.putStrLn

withCabalInfoFromStdin ∷ ToJSON a ⇒ (CabalInfo → IO a) → IO ()
withCabalInfoFromStdin proc = do
  unit ← JSON.decode <$> LBS.getContents
  maybe usage (proc >=> dumpJSON) $ getCabalInfo <$> unit

usage ∷ IO ()
usage = do
  progName ← T.pack <$> Sys.getProgName
  putStrLn "Usage:"
  putStrLn $ T.concat ["    ", progName, " scan"]
  putStrLn $ T.concat ["    ", progName, " graph < sourceUnit"]
  putStrLn $ T.concat ["    ", progName, " depresolve < sourceUnit"]
  putStrLn progName

srclibRun ∷ [Text] → IO ()
srclibRun ("scan":_) = C.scan >>= dumpJSON
srclibRun ["graph"] = withCabalInfoFromStdin graph
srclibRun ["depresolve"] = withCabalInfoFromStdin depresolveCmd
srclibRun _ = usage

main ∷ IO ()
main = getArgs >>= srclibRun
-}

-- Bullshit taken out of Cabal.hs --------------------------------------------

fromRight ∷ Either Text b → b
fromRight (Left x) = error $ T.unpack x
fromRight (Right y) = y

bindLeft ∷ a → Maybe b → Either a b
bindLeft x Nothing = Left x
bindLeft _ (Just y) = Right y

{-
repoFiles ∷ IO [Loc.RepoPath]
repoFiles = do
  root∷FilePath ← (Path.fromText <<< T.pack) <$> Sys.getCurrentDirectory
  let fQuery = Find.fileType ==? Find.RegularFile
  files ← map pFromStr <$> Find.find Find.always fQuery (pToStr root)
  flip mapM files $ \fn → do
    let pathText ∷ FilePath → Either Text Loc.RepoPath
        pathText p = do
          let root' = pFromStr(pToStr root <> "/")
          relative ← bindLeft "Bad path" (Path.stripPrefix root' p)
          textified ← Path.toText relative
          let parseError = "Invalid relative path: " <> textified
          parsed ← bindLeft parseError $ Loc.parseRelativePath textified
          return $ Loc.Repo parsed
    return $ fromRight $ pathText fn
-}

pToStr ∷ FilePath → String
pFromStr ∷ String → FilePath
(pToStr,pFromStr) = (Path.encodeString, Path.decodeString)

allRepoFiles ∷ FilePath → IO (Map Loc.RepoPath FilePath)
allRepoFiles root = do
  let pathText ∷ FilePath → Either Text Loc.RepoPath
      pathText p = do
        let root' = pFromStr(pToStr root <> "/")
        relative ← bindLeft "Bad path" (Path.stripPrefix root' p)
        textified ← Path.toText relative
        let parseError = "Invalid relative path: " <> textified
        parsed ← bindLeft parseError $ Loc.parseRelativePath textified
        return $ Loc.Repo parsed
      toRepoPath = pathText >>> fromRight

  let fQuery = Find.fileType ==? Find.RegularFile
  files ← map pFromStr <$> Find.find Find.always fQuery (pToStr root)
  return $ M.fromList $ (\f→(toRepoPath f, f)) <$> files

scan ∷ IO [CabalInfo]
scan = do
  root ← (Path.fromText <<< T.pack) <$> Sys.getCurrentDirectory
  traceM $ "root" <> show root
  fdb ← allRepoFiles root
  traceM $ "fdb" <> show fdb
  cabalFiles ← mapM readFile $ M.filterWithKey (\k _→C.isCabalFile k) fdb
  traceM $ "cabalFiles" <> show(M.keys cabalFiles)
  let repo = C.Repo fdb cabalFiles
  traceM $ show $ C.analyse repo
  return $ M.elems $ snd $ C.analyse repo

{-
  repo ← fmap M.fromList $ flip mapM files $ \fn → do
    content ← readFile fn
    let pathText ∷ FilePath → Either Text Loc.RepoPath
        pathText p = do
          let root' = toPath(toStr root <> "/")
          relative ← bindLeft "Bad path" (Path.stripPrefix root' p)
          textified ← Path.toText relative
          let parseError = "Invalid relative path: " <> textified
          parsed ← bindLeft parseError $ Loc.parseRelativePath textified
          return $ Loc.Repo parsed
    return (fromRight(pathText fn), content)
  return $ M.elems $ snd $ C.analyse repo
-}

testScan ∷ IO ()
testScan = scan >>= flip forM_ print

main = testScan
