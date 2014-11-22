-- TODO I'm using flattenPackageDescription because it's easy, but it might
--      not consistently give correct results.
-- TODO The scan command currently doesn't output enough dependency
--      information. This isn't technically incorrect, but it will need to
--      be fixed when the depresolve command is implemented.

{-# LANGUAGE UnicodeSyntax, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Functor
import Data.Maybe

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getProgName)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity as Verbosity

import System.FilePath.Find as Path
import System.Path as Path hiding (FilePath)


-- Source Unit ---------------------------------------------------------------

-- All paths are relative to repository root.
data CabalInfo = CabalInfo
   { cabalFile ∷ RelFile
   , cabalPkgName ∷ String
   , cabalDependencies ∷ [Dependency]
   , cabalSrcFiles ∷ [RelFile]
   , cabalSrcDirs ∷ [RelDir]
   } deriving (Show)

instance ToJSON Dependency where
  toJSON (Dependency (PackageName nm) _) = toJSON nm

instance ToJSON CabalInfo where
  toJSON (CabalInfo path _ deps files dirs) =
    let dir = dropFileName path in
      object [ "Type" .= ("HaskellPackage"∷String)
             , "Ops" .= object ["graph" .= Null, "depresolve" .= Null]
             , "Name" .= getPathString path
             , "Dir" .= getPathString dir
             , "Globs" .= map (getPathString >>> (++ "/**/*.hs")) dirs
             , "Files" .= map getPathString files
             , "Dependencies" .= deps
             , "Info" .= Null
             , "Repo" .= Null
             , "Config" .= Null
             ]


-- Scaning Repos and Parsing Cabal files -------------------------------------

findFiles ∷ FindClause Bool → AbsDir → IO [AbsFile]
findFiles q root = do
  fileNames ← find always (fileType ==? RegularFile &&? q) $ getPathString root
  return $ map asAbsPath fileNames

allDeps ∷ PackageDescription → [Dependency]
allDeps desc = buildDepends desc ++ (concat $ map getDeps $ allBuildInfo desc)
  where getDeps build = concat [ buildTools build
                               , pkgconfigDepends build
                               , targetBuildDepends build
                               ]

sourceDirs ∷ PackageDescription → [RelDir]
sourceDirs desc = map asRelDir $ librarySourceFiles ++ executableSourceFiles
  where librarySourceFiles =
          concat $ maybeToList $ (libBuildInfo>>>hsSourceDirs) <$> library desc
        executableSourceFiles =
          concat $ (buildInfo>>>hsSourceDirs) <$> executables desc

readCabalFile ∷ AbsDir → AbsFile → IO CabalInfo
readCabalFile repoDir cabalFilePath = do
  genPkgDesc ← readPackageDescription Verbosity.deafening $ show cabalFilePath
  let desc = flattenPackageDescription genPkgDesc
      PackageName name = pkgName $ package desc
      dirs = map (combine $ takeDirectory cabalFilePath) $ sourceDirs desc

  sourceFiles ← concat <$> (sequence $ map(findFiles $ extension==?".hs") dirs)
  return $ CabalInfo { cabalFile = makeRelative repoDir cabalFilePath
                     , cabalPkgName = name
                     , cabalDependencies = allDeps desc
                     , cabalSrcFiles = map (makeRelative repoDir) sourceFiles
                     , cabalSrcDirs = map (makeRelative repoDir) dirs
                     }

scanCmd ∷ IO ()
scanCmd = do
  cwd ← getCurrentDirectory
  let root = asAbsDir cwd
  cabalFiles ← findFiles (extension ==? ".cabal") root
  cabalInfos ← sequence $ map (readCabalFile root) cabalFiles
  BC.putStrLn $ encode cabalInfos

graphCmd ∷ IO ()
graphCmd = do
  putStr "{\"Docs\":[], \"Refs\":[], \"Defs\":[]}"

depresolveCmd ∷ IO ()
depresolveCmd = do
  putStr "\
    \[{\"Raw\":{\"name\":\"foo\"},\
    \  \"Target\":{\"ToRepoCloneURL\":\"https://github.com/example/repo\"}}]\
    \"

usage ∷ String → IO ()
usage cmd = do
  putStrLn "Usage:"
  putStrLn $ concat ["    ", cmd, " scan"]
  putStrLn $ concat ["    ", cmd, " graph"]
  putStrLn $ concat ["    ", cmd, " depresolve"]

main ∷ IO ()
main = getArgs >>= \case
  "scan":_ → scanCmd
  ["graph"] → graphCmd
  ["depresolve"] → depresolveCmd
  _ → getProgName >>= usage
