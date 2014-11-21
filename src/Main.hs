{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

module Main where

import System.Environment

scanCmd ∷ IO ()
scanCmd = putStr "\
  \[{\"Type\":\"HaskellPackage\",\
  \  \"Name\":\"myunit\",\
  \  \"Dependencies\":[{\"name\":\"foo\"}],\
  \  \"Ops\":{\"graph\":null,\"depresolve\":null}}]"

graphCmd ∷ IO ()
graphCmd = putStr "{\"Docs\":[], \"Refs\":[], \"Defs\":[]}"

depresolveCmd ∷ IO ()
depresolveCmd = putStr "\
  \[{\"Raw\":{\"name\":\"foo\"},\
  \  \"Target\":{\"ToRepoCloneURL\":\"https://github.com/example/repo\"}}]\
  \"

usage ∷ String → IO ()
usage cmd = do
  putStrLn "Usage:"
  putStrLn $ concat ["    ", cmd, " scan"]
  putStrLn $ concat ["    ", cmd, " graph"]
  putStrLn $ concat ["    ", cmd, " depresolve"]

main ∷ IO ()
main = getArgs >>= \case
  "scan":_ → scanCmd
  ["graph"] → graphCmd
  ["depresolve"] → depresolveCmd
  _ → getProgName >>= usage
