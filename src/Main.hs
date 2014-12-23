{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main where

import Types


import           Control.Arrow
import           Control.Monad
import           Data.Monoid

import qualified Data.ByteString.Lazy                          as LBS
import qualified Data.ByteString.Lazy.Char8                    as BC
import           Data.Maybe
import           Data.String
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text as Text
import           Data.Text (Text)

import           Test.QuickCheck
import           Data.Aeson                                    as JSON
import           Data.List.Split

import qualified Distribution.Verbosity                        as Verbosity
import           Shelly hiding (FilePath,path,(</>),(<.>),canonicalize)
import qualified System.Directory                              as Sys
import qualified System.Environment                            as Sys
import           System.FilePath.Find                          ((&&?), (==?))
import qualified System.FilePath.Find                          as P
import           System.IO
import           System.IO.Error
import qualified System.Path                                   as P
import           System.Path ((</>),(<.>))
import           System.Posix.Process                          (getProcessID)

import           Distribution.Package                          as C
import           Distribution.PackageDescription               as C
import           Distribution.PackageDescription.Configuration as C
import           Distribution.PackageDescription.Parse         as C
import qualified Documentation.Haddock                         as H
import           GHC
import           Name

findFiles ∷ P.FindClause Bool → P.AbsDir → IO [P.AbsFile]
findFiles q root = do
  let cond = P.fileType ==? P.RegularFile &&? q
  fileNames ← P.find P.always cond $ P.getPathString root
  return $ map P.asAbsPath fileNames

allDeps ∷ PackageDescription → Set RawDependency
allDeps desc = Set.fromList $ map toRawDep deps
  where deps = buildDepends desc ++ concatMap getDeps (allBuildInfo desc)
        toRawDep (C.Dependency (PackageName nm) _) = RawDependency nm
        getDeps build = concat [ buildTools build
                               , pkgconfigDepends build
                               , targetBuildDepends build
                               ]

sourceDirs ∷ PackageDescription → [P.RelDir]
sourceDirs desc = map P.asRelDir $ librarySourceFiles ++ executableSourceFiles
  where librarySourceFiles =
          concat $ maybeToList $ (libBuildInfo>>>hsSourceDirs) <$> library desc
        executableSourceFiles =
          concat $ (buildInfo>>>hsSourceDirs) <$> executables desc

readCabalFile ∷ P.AbsDir → P.AbsFile → IO SourceUnit
readCabalFile repoDir cabalFilePath = do
  genPkgDesc ← readPackageDescription Verbosity.deafening $ show cabalFilePath
  let desc = flattenPackageDescription genPkgDesc
      PackageName name = pkgName $ package desc
      dirs = map (P.combine $ P.takeDirectory cabalFilePath) $ sourceDirs desc

  sourceFiles ← concat <$> mapM (findFiles$P.extension==?".hs") dirs
  return SourceUnit { cabalFile = P.makeRelative repoDir cabalFilePath
                    , cabalPkgName = name
                    , cabalDependencies = allDeps desc
                    , cabalSrcFiles = Set.fromList $ map (P.makeRelative repoDir) sourceFiles
                    , cabalSrcDirs = Set.fromList $ map (P.makeRelative repoDir) dirs
                    }

resolve ∷ RawDependency → IO ResolvedDependency
resolve (RawDependency d) = return (ResolvedDependency d)

modulePathFromName ∷ String → ModulePath
modulePathFromName nm =
  case reverse $ splitOn "." nm of
    [] → error "Empty string is not a valid module name!"
    leaf:reversedPath → (reverse reversedPath, leaf)

moduleFileName ∷ ModulePath → P.RelFile
moduleFileName (path,nm) = relDirPath </> filename
  where relDirPath∷P.RelDir = foldl (</>) P.currentDir $ P.asRelPath<$>path
        filename∷P.RelFile = P.asRelFile nm <.> "hs"

findModuleFile ∷ [P.RelDir] → ModulePath → IO (Maybe P.RelFile)
findModuleFile rootDirs modul = do
  viablePaths ← flip mapM rootDirs $ \r → do
    let fn∷P.RelFile = r </> moduleFileName modul
    exists ← Sys.doesFileExist $ show fn
    return $ if exists then Just fn else Nothing
  return $ listToMaybe $ catMaybes viablePaths

-- TODO Stop silently ignoring errors. Specifically, EOF errors and invalid
--      lines and columns. Also, handle the case where the file doesn't exist.
getLocOffset ∷ FilePath → (Int,Int) → IO Integer
getLocOffset path (line,col) =
  flip catchIOError (\e → if isEOFError e then return 0 else ioError e) $
    withFile path ReadMode $ \h → do
      let (lineOffset, colOffset) = (max (line-1) 0, max (col-1) 0)
      replicateM_ lineOffset $ hGetLine h
      replicateM_ colOffset $ hGetChar h
      hTell h

srcSpanLoc ∷ FilePath → SrcSpan → IO (Maybe Loc)
srcSpanLoc _ (UnhelpfulSpan _) = return Nothing
srcSpanLoc fn (RealSrcSpan r) = do
  let l1 = srcSpanStartLine r
      c1 = srcSpanStartCol r
      l2 = srcSpanEndLine r
      c2 = srcSpanEndCol r
  startOffset ← getLocOffset fn (l1,c1)
  endOffset ← getLocOffset fn (l2,c2)
  return $ Just (fn,startOffset,endOffset)


-- Toolchain Command-Line Interface ------------------------------------------

scanCmd ∷ IO [SourceUnit]
scanCmd = do
  cwd ← Sys.getCurrentDirectory
  let root = P.asAbsDir cwd
  cabalFiles ← findFiles (P.extension ==? ".cabal") root
  mapM (readCabalFile root) cabalFiles

-- TODO This is not exception safe! Use a bracket?
withWorkingDirectory ∷ P.AbsRelClass ar ⇒ P.DirPath ar → IO a → IO a
withWorkingDirectory dir action = do
  oldDir ← Sys.getCurrentDirectory
  Sys.setCurrentDirectory(P.getPathString dir)
  result ← action
  Sys.setCurrentDirectory oldDir
  return result

nameDef ∷ SourceUnit → Name → IO(Maybe Def)
nameDef info nm = do
  let modul = nameModule nm
      srcSpan = nameSrcSpan nm
      modName = moduleNameString $ moduleName modul
      nameStr = occNameString $ getOccName nm

  fnMay ← findModuleFile (Set.toList $ cabalSrcDirs info) $ modulePathFromName modName
  let fn = show $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  loc ← fromMaybe (fn,0,0) <$> srcSpanLoc fn srcSpan
  return $ Just $ Def (splitOn "." modName, nameStr) nameStr Value loc

moduleDef ∷ SourceUnit → H.InstalledInterface → IO Def
moduleDef info iface = do
  let modNm∷String = moduleNameString $ moduleName $ H.instMod iface
      modPath = modulePathFromName modNm
  fnMay ← findModuleFile (Set.toList $ cabalSrcDirs info) modPath
  let fn = show $ fromMaybe (P.asRelPath "UNKNOWN") fnMay
  return $ Def modPath modNm Module (fn,0,0)

-- type Pkg = String
-- type ModuleNm = String
-- type FilePos = (Int,Int)
-- data NameSpc = Value | Type
-- data FileLoc = FileLoc FilePath FilePos FilePos
-- data GlobalBinding = Global String NameSpc ModuleNm Pkg
-- data LocalBinding = Local String NameSpc FileLoc
-- type SymGraphFile = [(FilePath, ModuleNm, Pkg, SymGraph)]
-- data SymGraph = SymGraph
--   { sgReferences :: [(FileLoc,(Either LocalBinding GlobalBinding))]
--   , sgExports :: [LocalBinding]
--   }
-- type Loc = (FilePath,Integer,Integer) -- A filename with a spaned formed by two byte offsets.
-- type ModulePath = ([String],String)
-- data RawDependency = RawDependency String
-- data ResolvedDependency = ResolvedDependency String
-- All paths in a SourceUnit should be relative to the repository root.
-- data SourceUnit = SourceUnit
--    { cabalFile         ∷ P.RelFile
--    , cabalPkgName      ∷ String
--    , cabalDependencies ∷ Set RawDependency
--    , cabalSrcFiles     ∷ Set P.RelFile
--    , cabalSrcDirs      ∷ Set P.RelDir
--    }
-- data DefKind = Module | Value | Type
-- data Def = Def { defModule ∷ ModulePath
--                , defName   ∷ String
--                , defKind   ∷ DefKind
--                , defLoc    ∷ Loc
--                }

defFromLocalBinding ∷ H.LocalBinding → Def
defFromLocalBinding (H.Local n kind (H.FileLoc fp start end)) = Def m n k l
  where m = "TODO"
        k = case kind of { H.Value→Value; H.Type→Type }
        l = undefined

defsFromHaddock ∷ SourceUnit → SymGraphFile → IO [Def]
defsFromHaddock info modules = concat $ mapM moduleDefs modules
  where moduleDefs (fp,modNm,pkg,gr) =
          return $ defFromLocalBinding $ H.sgExports gr

  -- exportedDefs' ← mapM (nameDef info) $ H.instExports iface
  -- let exportedDefs = catMaybes exportedDefs'
  -- modDef ← moduleDef info iface
  -- return $ modDef : exportedDefs

-- TODO escape ‘v’!
mkParam :: ∀m.(Monoid m,IsString m) ⇒ m → m → m
mkParam k v = "--" <> k <> "=" <> v <> "" -- Escape v!

graphCmd ∷ SourceUnit → IO Graph
graphCmd info = do
  pid ← toInteger <$> getProcessID

  let mktemp n = "/tmp/srclib-haskell-" <> n <> "." <> fromString(show pid)
      symbolGraph = mktemp "symbol-graph"
      sandbox = mktemp "sandbox"
      buildDir = mktemp "build-directory"

  let toStderr = log_stdout_with $ (Text.unpack >>> hPutStrLn stderr)
  let cabal_ = run_ "cabal"

  shelly $ toStderr $ do
    cd $ fromText $ fromString $ P.getPathString $ P.dropFileName $ cabalFile info
    cabal_ ["sandbox", "init", mkParam "sandbox" sandbox]
    cabal_ ["install", "--only-dependencies"]
    cabal_ ["configure", mkParam "builddir" buildDir]
    cabal_ [ "haddock", "--executables", "--internal"
           , mkParam "haddock-options" ("-G" <> symbolGraph)
           , mkParam "builddir" buildDir
           ]

  gr ← readFile $ Text.unpack symbolGraph
  haddockDefs ← defsFromHaddock info gr
  return $ Graph $ haddockDefs

depresolveCmd ∷ SourceUnit → IO [ResolvedDependency]
depresolveCmd = cabalDependencies >>> Set.toList >>> mapM resolve

dumpJSON ∷ ToJSON a ⇒ a → IO ()
dumpJSON = encode >>> BC.putStrLn

withSourceUnitFromStdin ∷ ToJSON a ⇒ (SourceUnit → IO a) → IO ()
withSourceUnitFromStdin proc = do
  unit ← JSON.decode <$> LBS.getContents
  maybe usage (proc >=> dumpJSON) unit

usage ∷ IO ()
usage = do
  progName ← Sys.getProgName
  putStrLn "Usage:"
  putStrLn $ concat ["    ", progName, " scan"]
  putStrLn $ concat ["    ", progName, " graph < sourceUnit"]
  putStrLn $ concat ["    ", progName, " depresolve < sourceUnit"]

srclibRun ∷ [String] → IO ()
srclibRun ("scan":_) = scanCmd >>= dumpJSON
srclibRun ["graph"] = withSourceUnitFromStdin graphCmd
srclibRun ["depresolve"] = withSourceUnitFromStdin depresolveCmd
srclibRun _ = usage

main ∷ IO ()
main = Sys.getArgs >>= srclibRun

test ∷ IO ()
test = quickCheck prop_cabalInfoJson
