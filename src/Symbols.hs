{-# LANGUAGE UnicodeSyntax #-}

module Symbols(findSymbols,SymbolInfo(..),joinL,SymbolKind(..)) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.State
import           Data.List           (intersperse,find)
import           Data.Maybe
import           DynFlags
import           FastString
import           GHC
import qualified GhcMonad            as GHC
import           GHC.Paths           (libdir)
import           OccName

type SrcPtr = (Int,Int)
data SymbolKind = Type | Value deriving Show
data SymbolInfo = SymbolInfo
  { si_nm   ∷ String
  , si_kind ∷ SymbolKind
  , si_loc  ∷ (String,SrcPtr,SrcPtr)
  }

-- TODO kind=Value is a lie, even with only exported symbols. However,
--      we're doing a temporary work-around to get something working before I
--      figure out how to walk the AST, and modInfoExports doesn't give us the
--      `Var` type that we would need in order to see what kind that it the
--      symbol represents.
extractSymInfo ∷ Name → Maybe SymbolInfo
extractSymInfo n = SymbolInfo nm kind <$> loc
  where kind = Value -- if isKind(varType n) then Type else Value
        nm = occNameString $ getOccName n
        loc = case nameSrcSpan (getName n) of
                UnhelpfulSpan _ → Nothing
                RealSrcSpan r → Just ( unpackFS $ srcSpanFile r
                                     , (srcSpanStartLine r, srcSpanStartCol r)
                                     , (srcSpanEndLine r, srcSpanEndCol r)
                                     )

joinL ∷ String → [String] → String
joinL x = intersperse x >>> concat

instance Show SymbolInfo where
  show (SymbolInfo n _ (f,(l,c),(l2,c2))) =
    joinL "@" [ n
              , joinL ":" [f,show l,show c]
                ++ "-"
                ++ joinL ":" (show<$>[l2,c2])]

handleGhcErrors ∷ IO a → IO a
handleGhcErrors = defaultErrorHandler defaultFatalMessager defaultFlushOut

getModuleSymbols ∷ GhcMonad m ⇒ ModSummary → m (String,[SymbolInfo])
getModuleSymbols ms = do
  m ← parseModule ms >>= typecheckModule
  let nm = moduleNameString $ ms_mod_name $ pm_mod_summary(tm_parsed_module m)
  let syms = catMaybes $ fmap extractSymInfo $ modInfoExports $ moduleInfo m
  return (nm,syms)

findSymbols ∷ (MonadIO m) ⇒ [FilePath]→FilePath→m(Maybe(String,[SymbolInfo]))
findSymbols otherincludes fp =
  liftIO . handleGhcErrors . runGhc (Just libdir) $ do
    dynflags ← getSessionDynFlags
    void $ setSessionDynFlags dynflags {
        includePaths = otherincludes ++ includePaths dynflags,
        importPaths = otherincludes ++ importPaths dynflags,
        ghcLink = NoLink,
        -- settings = (settings dynflags) {sTopDir=head otherincludes},
        packageFlags = [ExposePackage "ghc"]}
    target ← guessTarget fp Nothing
    setTargets [target]
    void $ load LoadAllTargets
    deps ← depanal [] False
    let ms = find (ms_hspp_file>>>(==fp)) deps
    case ms of
      Nothing → return Nothing
      Just x → Just <$> getModuleSymbols x
