-- TODO How can I tell if something is exported?
-- TODO How can I tell if a binding is global?
--   I can tell if a reference is to a global or local based on the
--     `(Scoped ...)` GlobalSymbol means global, TypeVar/LocalValue
--     means not global.

{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable, FlexibleInstances, LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax                                           #-}

module Resolve where

import ClassyPrelude hiding (concat, foldl', forM, forM_, mapM, mapM_, sum,
                      toList)

import qualified Prelude          as P
import           Text.Show.Pretty

import           Control.DeepSeq
import           Data.Foldable
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import qualified Data.Text        as T
import qualified Data.List        as L
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
import           Srclib (IdName,Kind)
import qualified Locations as Loc
import           Locations (LineCol(LineCol))

import Text.Printf


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

type Lc = LineCol
type STP = SrcTreePath
type MName = ModuleName

data Node loc path = Ref loc path | Def loc path
  deriving (Show,Ord,Eq)

data LocalNode loc lPath gPath = Bind loc gPath
                               | GRef loc gPath
                               | LRef loc lPath
                               | LExport loc gPath
  deriving (Show,Ord,Eq)

type ScrapedNode = LocalNode (Lc,Lc)
                             Lc
                             (MName,IdName,Srclib.Kind)

type ResolvedPaths = Node (MName,Lc,Lc)
                          (MName,IdName,Srclib.Kind,Maybe Lc)

type ResolvedFileNames = Node (STP,MName,Lc,Lc)
                              (MName,IdName,Srclib.Kind,Maybe Lc)

type ResolvedPackages = Node (Pkg,STP,MName,Lc,Lc)
                             (MName,IdName,Srclib.Kind,Maybe Lc)

type ResolvedOffsets = Node (Pkg,STP,MName,Int,Int)
                            (MName,IdName,Srclib.Kind,Maybe Int)

{-
Make refs for each import.
Make refs for each export.
Make defs for module headers.
Make bogus defs for each module that doesn't have a declaration.
Make bogus refs for each def.
For each module, insert a module def.
Convert from (MultiMap MName Node) -> (Set Ref, Set Def)
-}

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

extractNode mName = f
  where
    f ValueBinder          si n = [Bind (span si) (mName,t n,Srclib.Value)]
    f TypeBinder           si n = [Bind (span si) (mName,t n,Srclib.Type)]
    f (LocalValue loc)     si _ = [LRef (span si) (locLc loc)]
    f (TypeVar loc)        si _ = [LRef (span si) (locLc loc)]
    f (GlobalSymbol sym _) si _ = [GRef (span si) (symPath sym)]
    f (Export syms)        si _ = (\s→LExport (span si) (symPath s)) <$> syms
    f _                    _  _ = []

    span ∷ SrcSpanInfo → (Lc,Lc)
    span (SrcSpanInfo (SrcSpan _ l1 c1 l2 c2) _) = (LineCol l1 c1,LineCol l2 c2)

    nameStr (Language.Haskell.Exts.Syntax.Ident  s) = T.pack s
    nameStr (Language.Haskell.Exts.Syntax.Symbol s) = T.pack s

    cvtModuleName (Language.Haskell.Exts.Syntax.ModuleName n) = MN n

    locLc (SrcLoc fn l c) = LineCol l c

    t = T.pack

    symPath sym = (cvtModuleName (symbolModule sym),
                   nameStr (symbolName sym),
                   symbolKind sym)

nameNode ∷ MName → HSE.Name (Scoped SrcSpanInfo) → [ScrapedNode]
nameNode mName (Ident (Scoped info si) n) = extractNode mName info si n
nameNode mName (Symbol(Scoped info si) n) = extractNode mName info si n

qnStr ∷ QName a → [String]
qnStr (Qual    _ _ (Ident _ s))  = [s]
qnStr (Qual    _ _ (Symbol _ s)) = [s]
qnStr (UnQual  _   (Ident _ s))  = [s]
qnStr (UnQual  _   (Symbol _ s)) = [s]
qnStr (Special _ _)              = []

exportNode ∷ MName → HSE.ExportSpec (Scoped SrcSpanInfo) → [ScrapedNode]
exportNode mName ast = case ast of
  EVar (Scoped info si) _ qn            → concat $ extractNode mName info si <$> qnStr qn
  EAbs (Scoped info si) qn              → concat $ extractNode mName info si <$> qnStr qn
  EThingAll (Scoped info si) qn         → concat $ extractNode mName info si <$> qnStr qn
  EThingWith (Scoped info si) qn _      → concat $ extractNode mName info si <$> qnStr qn
  EModuleContents _ _                   → []

-- data Node loc lpath path = Ref loc path | Def loc path
-- data LocalNode loc lPath gPath = Bind loc gPath | GRef loc gPath | LRef loc lPath
-- type ScrapedNode = LocalNode (Lc,Lc) Lc (MName,IdName,Srclib.Kind)
-- type ResolvedPaths = Node (MName,Lc,Lc) (MName,IdName,Srclib.Kind,Maybe Lc)

bindings ∷ Foldable f => f (LocalNode (Lc,Lc) lpath path) → Map Lc path
bindings = foldl' ins M.empty
  where ins m (Bind (s,_) p) = M.insert s p m
        ins m _              = m

exports ∷ (Show path,Ord path,Foldable f) => f (LocalNode loc lpath path) → Set path
exports = traceShowId . foldl' ins S.empty
  where ins s (LExport _ p) = S.insert p s
        ins s _             = s

resolve ∷ Traversable t => MName → t ScrapedNode → t (Maybe ResolvedPaths)
resolve modu nodes = cvt <$> nodes

  where lookup ∷ Lc → Maybe ((MName,IdName,Srclib.Kind),Bool)

        tbl = fill <$> bindings nodes
        fill p = (p, S.member p (exports nodes))
        lookup = flip M.lookup tbl
        isExported = fromMaybe False . fmap snd . lookup

        cvt (Bind (s,e) (m,n,k)) =
          Just $ Def (modu,s,e) $ if isExported s then (m,n,k,Nothing)
                                                  else (m,n,k,Just s)

        cvt (LExport loc path) = cvt (GRef loc path)
        cvt (GRef (s,e) (m,n,k)) = Just $ Ref (modu,s,e) (m,n,k,Nothing)
        cvt (LRef (s,e) defLoc) =
          case lookup defLoc of
            Just ((m,n,k),_) → Just $ Ref (modu,s,e) (m,n,k,Just defLoc)
            Nothing          → trace (printf "%s not found" (show defLoc))
                                 Nothing

  -- First, make a table of all resolved nodes.
  -- Then, map over the list converting all local references to global ones.
  -- If a reference can't be resolved, print a warning, and drop the ref.

nodes1 ∷ MName → NameResolvedModule → [ScrapedNode]
nodes1 m = concat . map (nameNode m) . listify (not . L.null . (nameNode m))

nodes2 ∷ MName → NameResolvedModule → [ScrapedNode]
nodes2 m = concat . map (exportNode m) . listify (not . L.null . (exportNode m))

nodes ∷ MName → NameResolvedModule → [ScrapedNode]
nodes m ast = nodes1 m ast ++ nodes2 m ast

type NameResolvedModule = Module (Scoped SrcSpanInfo)
type SomeModuleMonad a = ModuleT [Symbol] IO a

exImports = "/home/ben/go/src/sourcegraph.com/sourcegraph/srclib-haskell/testdata/case/haskell-module-import"
exHW = "/home/ben/go/src/sourcegraph.com/sourcegraph/srclib-haskell/testdata/case/haskell-hello-world"
exSimle = "/home/ben/go/src/sourcegraph.com/sourcegraph/srclib-haskell/testdata/case/haskell-simple-tests"
warpdep = ("/home/ben/warpdeps/" </>)

scrapeTest ex = do
  p <- mkStubProject ex

  -- mapM_ (P.putStrLn . ppShow) p
  let mn = MN "Hello"
  let graph = catMaybes $ resolve mn $ sort $ ordNub $ join $ (nodes mn) <$> toList p
  mapM_ P.print graph
  P.putStrLn $ printf "\nThe graph contains %d nodes." $ length $ graph

  return ()
