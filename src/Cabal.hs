{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE NoImplicitPrelude    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cabal where

import ClassyPrelude hiding ((</>),(<.>))
import qualified Locations as Loc
import           Srclib hiding (Graph,Def)
import           Control.Arrow
import           Control.Applicative
import qualified Data.Maybe                                    as M
import qualified Data.Either                                   as E
import qualified Data.ByteString.Lazy                          as LBS
import qualified Data.ByteString.Lazy.Char8                    as BC
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import           Test.QuickCheck
import           Data.Aeson                                    as JSON
import           Data.List.Split
import qualified Distribution.Verbosity                        as Verbosity
import qualified System.Directory                              as Sys
import qualified System.Environment                            as Sys
import           System.FilePath.Find                          ((&&?), (==?))
import qualified Filesystem.Path.CurrentOS                     as FSP
import qualified System.FilePath.Find                          as P
import           System.IO (hGetChar, hTell, IOMode(..))
import           System.IO.Error
import qualified System.Path                                   as P
import           System.Path ((</>),(<.>))
import           System.Posix.Process                          (getProcessID)
import           Distribution.Package                          as Cabal
import           Distribution.PackageDescription               as Cabal
import           Distribution.PackageDescription.Configuration as Cabal
import           Distribution.PackageDescription.Parse         as Cabal

data CabalInfo = CabalInfo
  { cabalFile         ∷ P.RelFile
  , cabalPkgName      ∷ Text
  , cabalDependencies ∷ Set Text
  , cabalSrcFiles     ∷ Set P.RelFile
  , cabalSrcDirs      ∷ Set P.RelDir
  } deriving (Show, Eq)

findFiles ∷ P.FindClause Bool → P.AbsDir → IO [P.AbsFile]
findFiles q root = do
  let cond = P.fileType ==? P.RegularFile &&? q
  fileNames ← P.find P.always cond $ P.getPathString root
  return $ map P.asAbsPath fileNames

allDeps ∷ PackageDescription → Set Text
allDeps desc = Set.fromList $ toRawDep <$> deps
  where deps = buildDepends desc ++ concatMap getDeps (allBuildInfo desc)
        toRawDep (Cabal.Dependency (PackageName nm) _) = T.pack nm
        getDeps build = L.concat [ buildTools build
                                 , pkgconfigDepends build
                                 , targetBuildDepends build
                                 ]

sourceDirs ∷ PackageDescription → [P.RelDir]
sourceDirs desc = map P.asRelDir $ librarySourceFiles ++ executableSourceFiles
  where librarySourceFiles =
          concat $ maybeToList $ (libBuildInfo>>>hsSourceDirs) <$> library desc
        executableSourceFiles =
          concat $ (buildInfo>>>hsSourceDirs) <$> executables desc

readCabalFile ∷ P.AbsDir → P.AbsFile → IO CabalInfo
readCabalFile repoDir cabalFilePath = do
  genPkgDesc ← readPackageDescription Verbosity.deafening $ show cabalFilePath
  let desc = flattenPackageDescription genPkgDesc
      PackageName name = pkgName $ package desc
      dirs = map (P.combine $ P.takeDirectory cabalFilePath) $ sourceDirs desc

  sourceFiles ← concat <$> mapM (findFiles$P.extension==?".hs") dirs
  return CabalInfo { cabalFile = P.makeRelative repoDir cabalFilePath
                   , cabalPkgName = T.pack name
                   , cabalDependencies = allDeps desc
                   , cabalSrcFiles = Set.fromList $ P.makeRelative repoDir <$> sourceFiles
                   , cabalSrcDirs = Set.fromList $ P.makeRelative repoDir <$> dirs
                   }

fromRight ∷ Either a b → b
fromRight (Right x) = x

scanRepo ∷ FilePath → FilePath → IO (Maybe CabalInfo)
scanRepo repo cabal = Just <$> readCabalFile a b
  where a = P.asAbsDir $ T.unpack $ fromRight $ FSP.toText repo
        b = P.asAbsFile $ T.unpack $ fromRight $ FSP.toText cabal

scan ∷ IO [CabalInfo]
scan = do
  cwd ← Sys.getCurrentDirectory
  let root = P.asAbsDir cwd
  cabalFiles ← findFiles (P.extension ==? ".cabal") root
  mapM (readCabalFile root) cabalFiles

test ∷ IO ()
test = scan >>= flip forM_ print
