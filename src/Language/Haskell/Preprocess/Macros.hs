{-# LANGUAGE TemplateHaskell, UnicodeSyntax, QuasiQuotes #-}

module Language.Haskell.Preprocess.Macros(ghcMacros) where


import Data.Monoid

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Data.String.Here


ghcAutoConf ∷ String
ghcAutoConf = [hereFile|ghcautoconf.h|]

machDeps ∷ String
machDeps = [hereFile|MachDeps.h|]

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

ghcMacros ∷ String
ghcMacros = compilerMacros ++ ghcAutoConf ++ machDeps
