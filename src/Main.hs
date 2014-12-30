{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
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
depresolveCmd = undefined

getCabalInfo ∷ SourceUnit → CabalInfo
getCabalInfo = undefined

toSourceUnit ∷ CabalInfo → SourceUnit
toSourceUnit = undefined

instance ToJSON H.Graph where
 toJSON = undefined

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
