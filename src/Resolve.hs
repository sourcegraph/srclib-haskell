{-
TODO Convert from (MultiMap MName Node) -> (Set Ref, Set Def)
-}

{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable, FlexibleInstances, LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax, DeriveGeneric, OverloadedStrings         #-}

module Resolve where

import ClassyPrelude hiding (concat, foldl', forM, forM_, mapM, mapM_, sum,
                      toList, concatMap, span)

import qualified Prelude          as P
import           Text.Show.Pretty

import           Control.DeepSeq
import qualified Data.Bimap       as BM
import           Data.Bimap       (Bimap)
import           Data.Foldable
import qualified Data.List        as L
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import qualified Data.Text        as T
import           Data.Traversable

import Distribution.HaskellSuite.Modules as HP
import Language.Haskell.Exts.Annotated   as HSE hiding (ModuleName)
import Language.Haskell.Names            as HN

import Language.Haskell.Preprocess
import Language.Haskell.Preprocess.Internal

import qualified Language.Haskell.Exts.Annotated.Syntax as HSE

import           Data.Data (Data)
import qualified Data.Data as Data

import Data.Generics.Schemes

import qualified Language.Haskell.Exts.Syntax

import           Locations (LineCol (LineCol))
import qualified Locations as Loc
import           Srclib    (IdName, Kind)
import qualified Srclib

import Text.Printf

import Control.DeepSeq.Generics


-- Data Types ------------------------------------------------------------------

deriving instance Show Pkg

data EntireProject a = Proj { unProj ∷ Map SrcTreePath (Pkg, Map ModuleName a) }
  deriving (Functor,Foldable,Traversable,Show)

type MaybeModule = Maybe(Module SrcSpanInfo)

type NameResolvedModule = Module (Scoped SrcSpanInfo)
type SomeModuleMonad a = ModuleT [Symbol] IO a

-- | TODO This is poorly thought out.
type Transform a = Pkg → FilePath → String → a

type Lc = LineCol
type STP = SrcTreePath
type MName = ModuleName

data Node loc path = Ref loc path | Def loc path
  deriving (Show,Ord,Eq,Generic)

data LocalNode loc lPath gPath = Bind loc gPath
                               | GRef loc gPath
                               | LRef loc lPath
  deriving (Show,Ord,Eq,Generic)

type ScrapedNode = LocalNode (Lc,Lc)
                             Lc
                             (MName,IdName,Srclib.Kind)

type ResolvedNode = Node (MName,Lc,Lc)
                         (MName,IdName,Srclib.Kind,Lc)

type FilenamedNode = Node (STP,MName,Lc,Lc)
                              (MName,IdName,Srclib.Kind,Maybe Lc)

type PackagedNode = Node (Pkg,STP,MName,Lc,Lc)
                             (MName,IdName,Srclib.Kind,Maybe Lc)

type OffsetNode = Node (Pkg,STP,MName,Int,Int)
                       (MName,IdName,Srclib.Kind,Maybe Int)

instance NFData ScrapedNode
instance NFData ResolvedNode

allModules ∷ EntireProject (Module a) → [Module a]
allModules = toList -- Proj . join . fmap M.elems . fmap snd . M.elems . unProj

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

span ∷ SrcSpanInfo → (Lc,Lc)
span (SrcSpanInfo (SrcSpan _ l1 c1 l2 c2) _) = (LineCol l1 c1,LineCol l2 c2)


extractNode mName = f
  where
    f ValueBinder          si n = [Bind (span si) (mName,t n,Srclib.Value)]
    f TypeBinder           si n = [Bind (span si) (mName,t n,Srclib.Type)]
    f (LocalValue loc)     si _ = [LRef (span si) (locLc loc)]
    f (TypeVar loc)        si _ = [LRef (span si) (locLc loc)]
    f (GlobalSymbol sym _) si _ = [GRef (span si) (symPath sym)]
    f (Export syms)        si _ = (\s→GRef (span si) (symPath s)) <$> syms
    f (ImportPart syms)    si _ = (\s→GRef (span si) (symPath s)) <$> syms
    f _                    _  _ = []

    nameStr (Language.Haskell.Exts.Syntax.Ident  s) = T.pack s
    nameStr (Language.Haskell.Exts.Syntax.Symbol s) = T.pack s

    cvtModuleName (Language.Haskell.Exts.Syntax.ModuleName n) = MN n

    locLc (SrcLoc fn l c) = LineCol l c

    t = T.pack

    symPath sym = (cvtModuleName (symbolModule sym),
                   nameStr (symbolName sym),
                   symbolKind sym)

nameNode ∷ MName → HSE.Name (Scoped SrcSpanInfo) → [ScrapedNode]
nameNode mName (Ident (Scoped i si) n) = extractNode mName i si n
nameNode mName (Symbol(Scoped i si) n) = extractNode mName i si n

qnStr ∷ QName a → [String]
qnStr (Qual    _ _ (Ident _ s))  = [s]
qnStr (Qual    _ _ (Symbol _ s)) = [s]
qnStr (UnQual  _   (Ident _ s))  = [s]
qnStr (UnQual  _   (Symbol _ s)) = [s]
qnStr (Special _ _)              = []

exportNode ∷ MName → HSE.ExportSpec (Scoped SrcSpanInfo) → [ScrapedNode]
exportNode mName ast = case ast of
  EVar (Scoped i s) _ qn        → concat(extractNode mName i s <$> qnStr qn)
  EAbs (Scoped i s) qn          → concat(extractNode mName i s <$> qnStr qn)
  EThingAll (Scoped i s) qn     → concat(extractNode mName i s <$> qnStr qn)
  EThingWith (Scoped i s) qn _  → concat(extractNode mName i s <$> qnStr qn)
  EModuleContents _ _           → []

moduleDefNode ∷ MName → NameResolvedModule → ScrapedNode
moduleDefNode m (Module _ head _ _ _) = Bind loc path
  where
    path = (m, T.pack(unModuleName m), Srclib.Module)
    loc = case head of
      Nothing → (LineCol 1 1, LineCol 1 1)
      Just (ModuleHead _ (HSE.ModuleName (Scoped _ s) _) _ _) → span s

moduleRefNode ∷ ImportDecl (Scoped SrcSpanInfo) → [ScrapedNode]
moduleRefNode (ImportDecl _ nm _ _ _ _ alias _) = resultNodes
  where extract (HSE.ModuleName (Scoped _ s) n) = (span s,n)
        importedModulePath = snd(extract nm)
        mkRef (loc,m) = GRef loc (MN m, T.pack m, Srclib.Module)
        resultNodes = case (extract nm, extract <$> alias) of
          ((span1,nm),Nothing)       → mkRef <$> [(span1,nm)]
          ((span1,nm),Just(span2,_)) → mkRef <$> [(span1,nm),(span2,nm)]

bindings ∷ (Ord p,Foldable f) => f (MName,LocalNode (Lc,Lc) x p) → (Bimap (MName,Lc) p)
bindings = foldl' ins BM.empty
  where ins acc (modu,Bind (s,_) p) = BM.insert (modu,s) p acc
        ins acc _                   = acc

resolve ∷ Traversable t => t (MName,ScrapedNode) → t (Maybe ResolvedNode)
resolve nodes = cvt <$> nodes

  where tbl = bindings nodes

        isGlobal = const False

        cvt (modu,Bind (s,e) (m,n,k))   = Just $ Def (modu,s,e) (m,n,k,s)

        cvt (modu,GRef (s,e) p@(m,n,k)) = case BM.lookupR p tbl of
              Just (defModu,defLoc) → Just $ Ref (modu,s,e) (defModu,n,k,defLoc)
              Nothing               →
                flip trace Nothing
                  (printf "global sym %s not found" (show p))

        cvt (modu,LRef (s,e) defLoc)    = case BM.lookup (modu,defLoc) tbl of
              Just (m,n,k) → Just $ Ref (modu,s,e) (m,n,k,defLoc)
              Nothing      → flip trace Nothing
                               (printf "local sym %s not found" (show (modu,defLoc)))

nodes1 ∷ MName → NameResolvedModule → [ScrapedNode]
nodes1 m = concat . map (nameNode m) . listify (not . L.null . (nameNode m))

nodes2 ∷ MName → NameResolvedModule → [ScrapedNode]
nodes2 m = concat . map (exportNode m) . listify (not . L.null . (exportNode m))

nodes3 ∷ NameResolvedModule → [ScrapedNode]
nodes3 = concat . map moduleRefNode . listify (not . L.null . moduleRefNode)

nodes ∷ MName → NameResolvedModule → [ScrapedNode]
nodes m ast = L.concat [ nodes1 m ast
                       , nodes2 m ast
                       , nodes3 ast
                       , [moduleDefNode m ast]
                       ]

projMap ∷ EntireProject a → Map MName a
projMap = M.unions . map snd . M.elems . unProj

explode ∷ [(MName,[ScrapedNode])] → [(MName,ScrapedNode)]
explode = concatMap (\(k,vs) → [(k,v)|v←vs])

-- In Sourcegraph's model, each binding is also a reference to itself.
addDefRefs ∷ [(MName,ScrapedNode)] → [(MName,ScrapedNode)]
addDefRefs orig = extras ++ orig

  where extras = catMaybes $ cvt <$> orig

        cvt (m,n) = (m,) <$> defRef n

        defRef ∷ ScrapedNode → Maybe ScrapedNode
        defRef (Bind loc gPath) = Just $ GRef loc gPath
        defRef _                = Nothing


-- Testing Code ----------------------------------------------------------------

exImports = "/home/ben/go/src/sourcegraph.com/sourcegraph/srclib-haskell/testdata/case/haskell-module-import"
exHW = "/home/ben/go/src/sourcegraph.com/sourcegraph/srclib-haskell/testdata/case/haskell-hello-world"
exSimle = "/home/ben/go/src/sourcegraph.com/sourcegraph/srclib-haskell/testdata/case/haskell-simple-tests"
warpdep = ("/home/ben/warpdeps/" </>)

scrapeTest ex = do
  p <- mkStubProject ex

  mapM_ (P.putStrLn . ppShow) p

  let graph = ordNub $ addDefRefs $ explode $ M.toList $ M.mapWithKey nodes $ projMap p

  -- mapM_ P.print graph
  -- P.putStrLn "================================"

  let force x = x `deepseq` x
  let resolved = sort $ catMaybes $ resolve graph
  mapM_ P.print resolved
  P.putStrLn $ printf "\nThe graph contains %d nodes." $ length $ resolved

  return ()
