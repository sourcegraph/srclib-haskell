{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Imports(moduleRefs,ModuleRef,hello) where

import ClassyPrelude

import qualified Data.Text as T
import Language.Haskell.Exts.Annotated as HSE
import Language.Haskell.Exts.Extension as HSE
import qualified Locations as Loc
import qualified Examples as Ex

type Span = SrcSpanInfo
type ModuleRef = (String, (Int,Int), (Int,Int), Loc.ModulePath)

prToMaybe ∷ ParseResult a → Maybe a
prToMaybe (ParseOk x) = Just x
prToMaybe (ParseFailed l c) = flip trace Nothing $ mconcat["Parse failed! ", show l, "", show c]

allImports ∷ Module l → [ImportDecl l]
allImports (XmlPage _ _ _ _ _ _ _) = []
allImports (XmlHybrid _ _ _ _ _ _ _ _ _) = []
allImports (Module _ _ _ is _) = is

moduleDecl ∷ Module Span → Maybe (Span, Loc.ModulePath)
moduleDecl (Module _ Nothing _ _ _) = Nothing
moduleDecl (Module _ (Just (ModuleHead _ (ModuleName l n) _ _)) _ _ _) =
  Just (l, Loc.parseModulePath $ T.pack n)

importToModPath ∷ ImportDecl Span → (Span, Loc.ModulePath)
importToModPath (ImportDecl _ (ModuleName l n) _ _ _ _ _ _) =
  (l, Loc.parseModulePath $ T.pack n)

moduleRefs ∷ String → String → [ModuleRef]
moduleRefs fn source = cvt <$> results -- trace tree results
  where modul ∷ Maybe (Module Span)
        modul = prToMaybe $ parseFileContentsWithMode mode source
        tree = show modul
        imports = map importToModPath $ fromMaybe [] $ allImports <$> modul
        results = maybe imports (:imports) $ traceShowId $ join $ moduleDecl <$> modul
        mode = defaultParseMode
          { parseFilename     = fn
          , fixities          = Nothing
          , ignoreLinePragmas = False
          , extensions        = EnableExtension CPP : extensions defaultParseMode
          }
        cvt (SrcSpanInfo (SrcSpan fn sl sc el ec) _, mp) =
          (fn,(sl,sc),(el,ec),mp)

hello ∷ IO ()
hello = print $ moduleRefs "Main.hs" Ex.hello
