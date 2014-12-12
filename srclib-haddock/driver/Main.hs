module Main where

import           Documentation.Haddock (haddock)
import           System.Environment (getArgs)
import           System.IO

main :: IO ()
main = do
  hPutStrLn stderr "srclib-haddock!"
  hPutStrLn stderr "srclib-haddock!"
  hPutStrLn stderr "srclib-haddock!"
  hPutStrLn stderr "srclib-haddock!"
  getArgs >>= haddock
