{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Resolve where

import ClassyPrelude hiding (forM, forM_, mapM, mapM_, sum, foldl', toList)

import qualified Prelude as P
import Text.Show.Pretty

import           Control.DeepSeq
import           Data.Foldable
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import qualified Data.Text        as T
import           Data.Traversable

import Distribution.HaskellSuite.Modules as HP
import Language.Haskell.Exts.Annotated   as HSE hiding (ModuleName)
import Language.Haskell.Names            as HN

import           Language.Haskell.Preprocess
import           Language.Haskell.Preprocess.Internal

import qualified Language.Haskell.Exts.Annotated.Syntax as HSE

import qualified Data.Data as Data
import Data.Data (Data)

import Data.Generics.Schemes

import qualified Language.Haskell.Exts.Syntax

import qualified Srclib


-- Data Types ------------------------------------------------------------------

deriving instance Show Pkg

data EntireProject a = Proj { unProj ∷ Map SrcTreePath (Pkg, Map ModuleName a) }
  deriving (Functor,Foldable,Traversable,Show)

type MaybeModule = Maybe(Module SrcSpanInfo)

-- | TODO This is poorly thought out.
type Transform a = Pkg → FilePath → String → a

allModules ∷ EntireProject (Module a) → [Module a]
allModules = toList -- Proj . join . fmap M.elems . fmap snd . M.elems . unProj

emptyResolvedModule ∷ NameResolvedModule
emptyResolvedModule = undefined

evalSomeModuleMonad ∷ SomeModuleMonad a → IO a
evalSomeModuleMonad m = do
  evalModuleT m [] "don't use this" (\_fp → return [])

resolveNames ∷ EntireProject (Module SrcSpanInfo) → IO (EntireProject NameResolvedModule)
resolveNames proj =
  evalSomeModuleMonad $ do
    errors ← HN.computeInterfaces Haskell2010 [] (allModules proj)
    forM_ (S.toList errors) (traceM "" >> traceM . ("name resolution error: " <>) . show)
    forM proj $ HN.annotateModule Haskell2010 []

loadPkg ∷ NFData a ⇒
            FilePath → Transform a → SrcTreePath →
              IO (Pkg,Map ModuleName a)
loadPkg root transform pkgFile = do
  pkg ← scanPkg root pkgFile
  sources ← forM (pkgModules pkg) $ \fp →
    processFile root pkg (transform pkg $ unSTP fp) fp
  return (pkg,sources)

loadEntireProject ∷ NFData a ⇒ FilePath → Transform a → IO (EntireProject a)
loadEntireProject root transform = do
  pkgFiles ← S.toList <$> scan root
  p ← mapM (loadPkg root transform) $ M.fromList $ (\x→(x,x)) <$> pkgFiles
  return $ Proj p

parseEntireProject ∷ FilePath → IO (EntireProject MaybeModule)
parseEntireProject root = do
  pkgFiles ← S.toList <$> scan root
  p ← mapM (loadPkg root parseCode) $ M.fromList $ (\x→(x,x)) <$> pkgFiles
  return $ Proj p

listFiles ∷ FilePath → IO [FilePath]
listFiles root = do
  pkgFiles ← S.toList <$> scan root
  fmap (fmap unSTP . join) $ forM pkgFiles $ \pkgFile →
    M.elems . pkgModules <$> scanPkg root pkgFile

loc ∷ FilePath → IO Int
loc root = do
  pkgFiles ← S.toList <$> scan root
  fmap sum $ forM pkgFiles $ \pkgFile → do
    pkg ← scanPkg root pkgFile
    sum <$> forM (M.elems (pkgModules pkg))
              (processFile root pkg (length . P.lines))

configureAndParseEntireProject ∷ FilePath → IO (EntireProject MaybeModule)
configureAndParseEntireProject = flip analyseConfiguredCopy parseEntireProject

mapCatMaybes ∷ Ord k => Map k (Maybe v) → Map k v
mapCatMaybes  = M.fromList . foldl' f [] . M.toList
  where f a (_,Nothing) = a
        f a (k,Just v) = (k,v):a

unMaybeProj ∷ EntireProject MaybeModule → EntireProject(Module SrcSpanInfo)
unMaybeProj = Proj . fromProj
  where fromProj (Proj m) = fromPackage <$> m
        fromPackage (p,ms) = (p,mapCatMaybes ms)

mkStubProject ∷ FilePath → IO (EntireProject NameResolvedModule)
mkStubProject fp = do
  projs ← unMaybeProj <$> configureAndParseEntireProject fp
  resolveNames projs

-- the type (Scoped l) contains this constructor: ValueBinder
--   (Ident (Scoped (ValueBinder) (SrcSpanInfo
--                                  (SrcSpan "Data/Text.hs" 319 10 319 14)
--                                  ([])))
--      "arrA")

-- the type (Scoped l) contains this constructor: (LocalValue SrcLoc)
--   (Ident
--     (Scoped (LocalValue
--              (SrcLoc "/tmp/copy_for_analysis3131/src/Main.hs" 26 7))
--             SrcSpanInfo
--              { srcInfoSpan =
--                  SrcSpan "/tmp/copy_for_analysis3131/src/Main.hs" 27 28 27 32
--              , srcInfoPoints = []
--              })
--     "dump")))))

{-
(Ident
   (Scoped
      (GlobalSymbol
         Value
           { symbolModule = ModuleName "Hello" , symbolName = Ident "hello" }
         (Qual (ModuleName "Hello") (Ident "hello")))
      SrcSpanInfo
        { srcInfoSpan =
            SrcSpan "/tmp/copy_for_analysis3131/src/Main.hs" 5 8 5 19
        , srcInfoPoints = []
        })
   "hello"))))
-}

data Span = Span (Int,Int) (Int,Int)
  deriving (Eq,Show)

data Node = ValBind   Srclib.IdName Span
          | TypeBind  Srclib.IdName Span
          | GlobalRef Span          (ModuleName,Srclib.IdName,Srclib.Kind)
          | LocalRef  Span          SrcLoc
  deriving (Show)

symbolKind ∷ Symbol → Srclib.Kind
symbolKind sym = case sym of
  HN.Value{..}       → Srclib.Value
  HN.Method{..}      → Srclib.Value
  HN.Selector{..}    → Srclib.Value
  HN.Constructor{..} → Srclib.Value
  HN.Type{..}        → Srclib.Type
  HN.Data{..}        → Srclib.Type
  HN.NewType{..}     → Srclib.Type
  HN.TypeFam{..}     → Srclib.Type
  HN.DataFam{..}     → Srclib.Type
  HN.Class{..}       → Srclib.Type

nameNode ∷ HSE.Name (Scoped SrcSpanInfo) → Maybe Node
nameNode ast =
  let span ∷ SrcSpanInfo → Span
      span (SrcSpanInfo (SrcSpan _ l1 c1 l2 c2) _) = Span (l1,c1) (l2,c2)
      nameStr (Language.Haskell.Exts.Syntax.Ident  s) = T.pack s
      nameStr (Language.Haskell.Exts.Syntax.Symbol s) = T.pack s
      cvtModuleName (Language.Haskell.Exts.Syntax.ModuleName n) = MN (T.pack n)
      t = T.pack
      gRef si sym = GlobalRef (span si) ( cvtModuleName (symbolModule sym)
                                           , nameStr (symbolName sym)
                                           , symbolKind sym
                                           )
  in case ast of
    Ident (Scoped ValueBinder          si) n → Just $ ValBind (t n) (span si)
    Symbol(Scoped ValueBinder          si) n → Just $ ValBind (t n) (span si)
    Ident (Scoped (LocalValue loc)     si) _ → Just $ LocalRef (span si) loc
    Symbol(Scoped (LocalValue loc)     si) _ → Just $ LocalRef (span si) loc
    Ident (Scoped (GlobalSymbol sym _) si) _ → Just $ gRef si sym
    Symbol(Scoped (GlobalSymbol sym _) si) _ → Just $ gRef si sym
    _                                        → Nothing

nodes ∷ NameResolvedModule → [Node]
nodes = catMaybes . map nameNode . listify (isJust . nameNode)

type NameResolvedModule = Module (Scoped SrcSpanInfo)
type SomeModuleMonad a = ModuleT [Symbol] IO a

exImports = "/home/ben/go/src/sourcegraph.com/sourcegraph/srclib-haskell/testdata/case/haskell-module-import"
exHW = "/home/ben/go/src/sourcegraph.com/sourcegraph/srclib-haskell/testdata/case/haskell-hello-world"
exSimle = "/home/ben/go/src/sourcegraph.com/sourcegraph/srclib-haskell/testdata/case/haskell-simple-tests"
warpdep = ("/home/ben/warpdeps/" </>)
lol ex = do
  p <- mkStubProject ex

  -- mapM_ (P.putStrLn . ppShow) p
  -- mapM_ P.print $ join $ nodes <$> toList p

  P.print $ length $ join $ nodes <$> toList p

  return ()
