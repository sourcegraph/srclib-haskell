# preprocess-haskell
This is a library for preparing Haskell source code for analysis. It
solves these problems:

  - Scan directory trees to find Haskell packages.
  - Configure packages with `autotools`.
  - Pre-Process source code with `cpphs` (with macros generated from
    information in the .cabal file)
  - Parse pre-processed source code use `haskell-src-exts` (with the
    default extensions generated from information in the cabal file.)

## Documentation
See the API documentation on Hackage here:
https://hackage.haskell.org/package/preprocess-haskell


## External Dependencies
In order to handle codebases that need to be configured with `autotools`,
GNU autotools must be installed and available in your PATH.

## Status
Doing a full run against
[https://github.com/bsummer4/warpdep.git](`wai` and it's transitive dependencies),
results in the following (non-fatal) parse errors. Most of these seems
to be caused by incorrect code or because of limitations in the
`haskell-src-exts` library.

    "base/Data/OldTypeable/Internal.hs" 474 1 "Parse error: ;"
    "base/Data/Proxy.hs" 35 19 "Parse error: *"
    "base/Data/Type/Equality.hs" 165 9 "Parse error: a1"
    "base/GHC/PArr.hs" 34 8 "Parse error: :]"
    "base/GHC/TypeLits.hs" 176 24 "Illegal test declaration"
    "base/tests/CatPairs.hs" 14 21 "Parse error: x"
    "base/tests/IO/hGetChar001.hs" 14 12 "Parse error: Last statement in a do-block must be an expression"
    "base/tests/IO/misc001.hs" 22 3 "Parse error: Last statement in a do-block must be an expression"
    "base/tests/IO/newline001.hs" 112 11 "Parse error: Last statement in a do-block must be an expression"
    "bytestring/tests/FusionBench.hs" 26 36 "Parse error: \"noacc/filter\"#"
    "bytestring/tests/builder/Data/ByteString/Builder/Tests.hs" 335 1 "Parse error: ;"
    "bytestring/tests/FusionBench.hs" 26 36 "Parse error: \"noacc/filter\"#"
    "bytestring/tests/builder/Data/ByteString/Builder/Tests.hs" 335 1 "Parse error: ;"
    "containers/Data/Map/Base.hs" 343 1 "Parse error: ;"
    "containers/Data/Set/Base.hs" 246 1 "Parse error: ;"
    "directory/tests/copyFile002.hs" 14 11 "Parse error: Last statement in a do-block must be an expression"
    "ghc-prim/GHC/Tuple.hs" 26 1 "Illegal data/newtype declaration"
    "ghc-prim/GHC/Types.hs" 29 10 "Parse error: :"
    "integer-gmp/GHC/Integer/GMP/Prim.hs" 125 16 "Parse error: prim"
    "network/Network.hs" 356 9 "Parse error: Last statement in a do-block must be an expression"
    "stm/tests/stm049.hs" 48 11 "Parse error: Last statement in a do-block must be an expression"
    "text/tests/Tests/Properties.hs" 132 5 "Parse error: Last statement in a do-block must be an expression"
    "text/tests/Tests/Properties.hs" 132 5 "Parse error: Last statement in a do-block must be an expression"
    "wai/warp/test/RunSpec.hs" 360 13 "Parse error: Last statement in a do-block must be an expression"
