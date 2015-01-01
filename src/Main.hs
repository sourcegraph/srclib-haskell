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

import qualified System.Environment as Sys

import Cabal as C
import Haddock as H
import Srclib as Src

depresolveCmd ∷ CabalInfo → IO ()
depresolveCmd = error . show

getCabalInfo ∷ SourceUnit → CabalInfo
getCabalInfo = error . show

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


-- Bullshit taken out of Cabal.hs --------------------------------------------

loadRepo ∷ FilePath → IO Repo
loadRepo root = do
  let (toStr,toPath) = (P.encodeString, P.decodeString)
  fileNames ← toPath <$> Find.find Find.always Find.RegularFile $ toStr root
  files ← P.decodeString <$> fileNames
  fmap M.fromList $ flip mapM cabalFiles $ \f → do
    content ← hGetContents $ P.encodeString f
    return (f,content)

scanRepo ∷ FilePath → FilePath → IO (Maybe CabalInfo)
scanRepo repoDir cabalFile = do
  repo ← loadRepo repoDir
  let fileContents = Map.lookup (cabalFile repo)
  return $ fileContents >>= cabalInfo repo cabalFile

scan ∷ IO [CabalInfo]
scan = do
  root ← (P.fromText . T.pack) <$> Sys.getCurrentDirectory
  cabalFiles ← filesWithExt "cabal" root
  mapM (readCabalFile root) cabalFiles

testScan ∷ IO ()
testScan = scan >>= flip forM_ print
