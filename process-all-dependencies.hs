#!/usr/bin/env runhaskell

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- import Control.Monad
-- import Data.Functor
-- import Data.Maybe
-- import Data.Monoid
import Debug.Trace
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import BasicPrelude hiding (empty)
import Prelude.Unicode
import Turtle
import Network.URI
import qualified Filesystem.Path.CurrentOS as P

f & g = g $ f

parseVCSLine ∷ Text → Either Text (URI,Text)
parseVCSLine l = case T.split (≡'@') l of
  [uriStr,branch] → case parseURI (removeTrailingSlash $ T.unpack uriStr) of
    Nothing → Left l
    Just uri → Right (uri,branch)
  _ → Left l

getVCSInfo ∷ Text → Shell [Text]
getVCSInfo pkg = do
  vcs ← empty
      & inproc "cabal-db" ["vcs", pkg]
      & inproc "grep" ["://"]
      & inshell "sed -r 's:\\x1B\\[[0-9;]*[mK]::g; s:^ *::'"
  return $ lines vcs

stripDotGit x = fromMaybe x $ T.stripSuffix ".git" x

pathFromGitURI ∷ Text → Maybe Text
pathFromGitURI p = r $ reverse $ T.split (≡'/') p
  where r []      = Nothing
        r [""]    = Nothing
        r ("":xs) = r xs
        r (x:_)   = Just $ stripDotGit x

run ∷ Text → IO ExitCode
run x = do
  wd ← pwd
  echo $ T.pack $ concat["(", P.encodeString wd, ")$", T.unpack x]
  shell x empty

removeTrailingSlash x = fromMaybe x $ T.unpack <$> T.stripSuffix "/" (T.pack x)

printVCS ∷ (URI,Text) → IO ()
printVCS (uri,br) = do
  (pathFromGitURI $ T.pack $ uriPath uri) & \case
    Nothing → return()
    Just "zlib" → return()
    Just d → do
      h ← home
      echo $ "cd " <> T.pack(P.encodeString(h <> "warpdeps"))
      cd $ h <> "warpdeps"
      run $ "git clone " <> stripDotGit(show uri) <> ".git"
      ok ← testdir $ fromText d
      if not ok then return() else do
        cd $ fromText d
        run $ "git checkout " <> br
        run $ "src do-all -m program"
        run $ "src push"
        return()

main ∷ IO ()
main = sh $ do
  x ← map parseVCSLine <$> getVCSInfo "warp"
  forM (lefts x) $ traceM . T.unpack . ("Failed to parse VCS URI line: " <>)
  forM (rights x) $ liftIO . printVCS
