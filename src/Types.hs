{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Control.Arrow
import Control.Monad
import Data.Functor
import Data.Monoid

import qualified Data.Set as Set
import Data.List (intersperse)
import Data.Set (Set)
import Data.Text (Text)

import Data.Aeson as JSON
import qualified System.Path as P hiding (FilePath)
import Test.QuickCheck


-- Types ---------------------------------------------------------------------

-- TODO Making these both of Loc's offsets be unsigned integers, and making
--      the second number a size would make invalid ranges unrepresentables.

type Loc = (FilePath,Integer,Integer) -- A filename with a spaned formed by two byte offsets.
type ModulePath = ([String],String)
data RawDependency = RawDependency String
data ResolvedDependency = ResolvedDependency String

-- All paths in a SourceUnit should be relative to the repository root.
data SourceUnit = SourceUnit
   { cabalFile         ∷ P.RelFile
   , cabalPkgName      ∷ String
   , cabalDependencies ∷ Set RawDependency
   , cabalSrcFiles     ∷ Set P.RelFile
   , cabalSrcDirs      ∷ Set P.RelDir
   }

data Graph = Graph [Def]
data DefKind = Module | Value | Type
newtype Ref = Ref Def
data Def = Def { defModule ∷ ModulePath
               , defName   ∷ String
               , defKind   ∷ DefKind
               , defLoc    ∷ Loc
               }


-- Utilities -----------------------------------------------------------------

rawDependency ∷ ResolvedDependency → RawDependency
rawDependency (ResolvedDependency d) = RawDependency d

srclibHsUrl ∷ String
srclibHsUrl = "sourcegraph.com/sourcegraph/srclib-haskell"

cleanedPathStr ∷ P.RelPath fd → String
cleanedPathStr = canonicalize <<< P.getPathString

canonicalize ∷ FilePath -> FilePath
canonicalize ('.':'/':s) = canonicalize s
canonicalize s = s

modulePathString ∷ ModulePath → String
modulePathString (path,leaf) = joinL "." $ path ++ [leaf]

modulePathToSrclibPath ∷ ModulePath → String
modulePathToSrclibPath (path,leaf) = joinL "/" $ path ++ [leaf]

joinL ∷ ∀a. [a] → [[a]] → [a]
joinL sep = concat <<< intersperse sep


-- Basic Instances -----------------------------------------------------------

deriving instance Eq RawDependency
deriving instance Eq ResolvedDependency
deriving instance Eq SourceUnit

deriving instance Ord RawDependency
deriving instance Ord ResolvedDependency

deriving instance Show Def
deriving instance Show DefKind
deriving instance Show RawDependency
deriving instance Show ResolvedDependency
deriving instance Show SourceUnit


-- Arbitrary -----------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary(Set a) where
  arbitrary = Set.fromList <$> arbitrary

instance Arbitrary RawDependency where
  arbitrary = RawDependency <$> arbitrary

-- TODO pathtype's Gen instances output far too much data. Hack around it.
newtype PathHack a b = PathHack (P.Path a b)
instance Arbitrary (PathHack a b) where
  arbitrary = return $ PathHack $ P.asPath "./asdf"

unPathHack ∷ PathHack a b → P.Path a b
unPathHack (PathHack x) = x

instance Arbitrary SourceUnit where
  arbitrary = do
    file ← unPathHack <$> arbitrary
    files ← Set.fromList <$> map unPathHack <$> arbitrary
    dirs ← Set.fromList <$> map unPathHack <$> arbitrary
    deps ← arbitrary
    name ← arbitrary
    return $ SourceUnit file name deps files dirs


-- JSON ----------------------------------------------------------------------

instance ToJSON RawDependency where
  toJSON (RawDependency s) = toJSON s

instance ToJSON ResolvedDependency where
  toJSON dep@(ResolvedDependency nm) =
    object [ "Raw" .= rawDependency dep
           , "Target" .= object [ "ToRepoCloneURL" .= (""∷String)
                                , "ToUnit" .= nm
                                , "ToUnitType" .= ("HaskellPackage"∷String)
                                , "ToVersionString" .= (""∷String)
                                , "ToRevSpec" .= (""∷String)
                                ]
           ]

instance FromJSON SourceUnit where
 parseJSON (Object v) = do
    infoObj ← v .: "Data"
    case infoObj of
      Object info → do
        path ← P.asRelFile <$> info .: "Path"
        name ← v .: "Name"
        deps ← Set.map RawDependency <$> v .: "Dependencies"
        files ← Set.map P.asRelFile <$> v .: "Files"
        dirs ← Set.map P.asRelDir <$> info .: "Dirs"
        return $ SourceUnit path name deps files dirs
      _ → mzero
 parseJSON _ = mzero

instance ToJSON SourceUnit where
  toJSON (SourceUnit path name deps files dirs) =
    let dir = P.dropFileName path in
      object [ "Type" .= ("HaskellPackage"∷String)
             , "Ops" .= object
                 [ "graph" .= object [ "Toolchain" .= srclibHsUrl
                                     , "Subcmd" .= ("graph"∷Text)
                                     ]
                 , "depresolve" .= object [ "Toolchain" .= srclibHsUrl
                                          , "Subcmd" .= ("depresolve"∷Text)
                                          ]
                 ]
             , "Name" .= name
             , "Dir" .= cleanedPathStr dir
             , "Globs" .= Set.map (P.getPathString >>> (<> "/**/*.hs") >>> canonicalize) dirs
             , "Files" .= Set.map cleanedPathStr files
             , "Dependencies" .= deps
             , "Data" .= object [ "Path" .= cleanedPathStr path
                                , "Dirs" .= Set.map cleanedPathStr dirs
                                ]
             , "Repo" .= Null
             , "Config" .= Null
             ]

instance ToJSON Def where
  toJSON d = object [ "Path" .= (modulePathToSrclibPath $ defModule d)
                    , "TreePath" .= (modulePathToSrclibPath $ defModule d)
                    , "Name" .= defName d
                    , "Kind" .= show (defKind d)
                    , "File" .= canonicalize(case defLoc d of (fn,_,_)→fn)
                    , "DefStart" .= (case defLoc d of (_,s,_)→s)
                    , "DefEnd" .= (case defLoc d of (_,_,e)→e)
                    , "Exported" .= True
                    , "Test" .= False
                    , "JsonText" .= object[]
                    ]

instance ToJSON Ref where
  toJSON (Ref d) = object [ "DefRepo" .= (""∷Text)
                          , "DefUnitType" .= (""∷Text)
                          , "DefUnit" .= (""∷Text)
                          , "DefPath" .= (modulePathToSrclibPath $ defModule d)
                          , "Def" .= True
                          , "File" .= canonicalize(case defLoc d of (fn,_,_)→fn)
                          , "Start" .= (case defLoc d of (_,s,_)→s)
                          , "End" .= (case defLoc d of (_,_,e)→e)
                          ]

instance ToJSON Graph where
  toJSON (Graph defs) = object ["Docs".=e, "Refs".=(Ref<$>defs), "Defs".=defs]
    where e = []∷[String]


-- Quickcheck Properties -----------------------------------------------------

prop_cabalInfoJson ∷ SourceUnit → Bool
prop_cabalInfoJson c = (Just c==) $ JSON.decode $ JSON.encode c
