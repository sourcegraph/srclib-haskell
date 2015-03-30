{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Resolve where

import ClassyPrelude hiding (forM, forM_, mapM, mapM_, sum, foldl', toList)

import qualified Prelude as P

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


-- Data Types ------------------------------------------------------------------

data EntireProject a = Proj { unProj ∷ Map SrcTreePath (Pkg, Map ModuleName a) }
  deriving (Functor,Foldable,Traversable)

type MaybeModule = Maybe(Module SrcSpanInfo)

-- | TODO This is poorly thought out.
type Transform a = Pkg → FilePath → String → a

allModules ∷ EntireProject (Module a) → [Module a]
allModules = toList -- Proj . join . fmap M.elems . fmap snd . M.elems . unProj

type NameResolvedModule = Module (Scoped SrcSpanInfo)
type SomeModuleMonad a = ModuleT [Symbol] IO a

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

doAllThings ∷ FilePath → IO (EntireProject NameResolvedModule)
doAllThings fp = do
  projs ← unMaybeProj <$> configureAndParseEntireProject fp
  resolved ← resolveNames projs
  mapM_ (traceM . P.show . length . P.show) resolved
  return resolved
