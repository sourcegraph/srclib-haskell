{-# LANGUAGE QuasiQuotes #-}

module Examples where

import Data.String.Here

hello :: String
hello = [here|
{-# LANGUAGE NoExplicitPrelude, UnicodeSyntax #-}

module Main where

import Prelude
import Bogus.Module.Name
import Data.Text

-- | The program entry point. It prints "Hello World".
main ∷ IO ()
main = putStrLn "Hello World"
|]
