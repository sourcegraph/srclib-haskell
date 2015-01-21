{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Srclib where

import           ClassyPrelude
import           Locations       (RepoPath)
import qualified Locations       as L

import           Data.Aeson      as JSON
import           Test.QuickCheck


-- Types ---------------------------------------------------------------------

data SourceUnit = SourceUnit
  { suCabalFile    ∷ RepoPath
  , suPkgName      ∷ Text
  , suPkgDir       ∷ RepoPath
  , suDependencies ∷ [(Text,Text)]
  , suSrcDirs      ∷ [RepoPath]
  , suSrcFiles     ∷ [RepoPath]
  , suSrcGlobs     ∷ [Text]
  }

data ResolvedDependency = ResolvedDependency
  { depRaw             ∷ (Text,Text)
  , depToRepoCloneURL  ∷ Text
  , depToUnit          ∷ Text
  , depToVersionString ∷ Text
  , depToRevSpec       ∷ Text
  }

data Ref = Ref
  { refDefRepo     ∷ Text
  , refDefUnitType ∷ Text
  , refDefUnit     ∷ Text
  , refDefPath     ∷ Text
  , refIsDef       ∷ Bool
  , refFile        ∷ RepoPath
  , refStart       ∷ Int
  , refEnd         ∷ Int
  }

data Def = Def
  { defPath     ∷ Text
  , defTreePath ∷ Text
  , defName     ∷ Text
  , defKind     ∷ Text
  , defFile     ∷ RepoPath
  , defDefStart ∷ Int
  , defDefEnd   ∷ Int
  , defExported ∷ Bool
  , defTest     ∷ Bool
  }

data Graph = Graph [Def] [Ref]

instance Monoid Graph where
  mempty = Graph [] []
  (Graph a b) `mappend` (Graph α β) = Graph (mappend a α) (mappend b β)

deriving instance Show Ref
deriving instance Show Def
deriving instance Show Graph

deriving instance Eq SourceUnit
deriving instance Show SourceUnit

instance Arbitrary SourceUnit where
  arbitrary = SourceUnit <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                         <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Graph where
  arbitrary = Graph <$> arbitrary <*> arbitrary

instance Arbitrary Ref where
  arbitrary = Ref <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                  <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Def where
  arbitrary = Def <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                  <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                  <*> arbitrary

instance Arbitrary ResolvedDependency where
  arbitrary = ResolvedDependency <$> arbitrary <*> arbitrary <*> arbitrary
                                 <*> arbitrary <*> arbitrary

instance ToJSON RepoPath where
  toJSON r = toJSON $ L.srclibPath r

instance FromJSON RepoPath where
  parseJSON j = parseJSON j >>= f
    where f x = case L.parseRelativePath x of
                  Nothing → mzero
                  Just p → return $ L.Repo p

instance ToJSON Ref where
  toJSON r = object [ "DefRepo" .= refDefRepo r
                    , "DefUnitType" .= refDefUnitType r
                    , "DefUnit" .= refDefUnit r
                    , "DefPath" .= refDefPath r
                    , "Def" .= refIsDef r
                    , "File" .= refFile r
                    , "Start" .= refStart r
                    , "End" .= refEnd r
                    ]

instance ToJSON Def where
  toJSON d = object [ "Path" .= defPath d
                    , "TreePath" .= defTreePath d
                    , "Name" .= defName d
                    , "Kind" .= defKind d
                    , "File" .= defFile d
                    , "DefStart" .= defDefStart d
                    , "DefEnd" .= defDefEnd d
                    , "Exported" .= defExported d
                    , "Test" .= defTest d
                    , "JsonText" .= object[]
                    ]

instance ToJSON ResolvedDependency where
  toJSON d =
    object [ "Raw" .= depRaw d
           , "Target" .= object [ "ToRepoCloneURL" .= depToRepoCloneURL d
                                , "ToUnit" .= depToUnit d
                                , "ToVersionString" .= depToVersionString d
                                , "ToRevSpec" .= depToRevSpec d
                                , "ToUnitType" .= ("HaskellPackage"∷Text)
                                ]
           ]

instance ToJSON Graph where
  toJSON (Graph defs refs) = object [ "Defs" .= defs
                                    , "Refs" .= refs
                                    , "Docs" .= ([]∷[Text])
                                    ]

url ∷ Text
url = "sourcegraph.com/sourcegraph/srclib-haskell"

instance ToJSON SourceUnit where
  toJSON su = object
    [ "Type" .= ("HaskellPackage"∷Text)
    , "Name" .= suPkgName su
    , "Dir" .= suPkgDir su
    , "Globs" .= suSrcGlobs su
    , "Files" .= suSrcFiles su
    , "Dependencies" .= suDependencies su
    , "Data" .= object["CabalFile".=suCabalFile su, "Dirs".=suSrcDirs su]
    , "Repo" .= Null
    , "Config" .= Null
    , "Ops" .= object
        [ "graph" .=
            object ["Toolchain".=url, "Subcmd".=("graph"∷Text)]
        , "depresolve" .=
            object ["Toolchain".=url, "Subcmd".=("depresolve"∷Text)]
        ]
    ]

instance FromJSON SourceUnit where
  parseJSON (Object v) = do
    extraData ← v .: "Data" >>= \x→case x of {Object o→return o; _→mzero}
    SourceUnit <$> extraData .: "CabalFile"
               <*> v .: "Name"
               <*> v .: "Dir"
               <*> v .: "Dependencies"
               <*> extraData .: "Dirs"
               <*> v .: "Files"
               <*> v .: "Globs"
  parseJSON _ = mzero

prop_sourceUnitJSON ∷ SourceUnit → Bool
prop_sourceUnitJSON c = (Just c==) $ JSON.decode $ JSON.encode c

test ∷ IO ()
test = quickCheck prop_sourceUnitJSON
