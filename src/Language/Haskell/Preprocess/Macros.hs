{-# LANGUAGE TemplateHaskell, UnicodeSyntax, QuasiQuotes #-}

module Language.Haskell.Preprocess.Macros(compilerMacros,stdHdrs) where

import Data.Monoid

import Data.String.Here
import Data.FileEmbed

import qualified Data.Map as M
import qualified Filesystem.Path as P
import qualified Filesystem.Path.CurrentOS as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

stdHdrs ∷ M.Map P.FilePath BS.ByteString
stdHdrs = M.fromList $ map (\(k,v)→(P.decodeString k,v)) $(embedDir "include")

compilerMacros ∷ String
compilerMacros = x++"\n" where x = [here|
#define __GLASGOW_HASKELL__ 708
#define x86_64_HOST_ARCH 1
#define linux_HOST_OS 1
#define mingw32_HOST_OS 1
#define FLT_RADIX 2
#define HAVE_POLL 1
#define INTEGER_GMP 1
|]
