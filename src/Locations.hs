{-
  This module contains utilities for dealing with locations within
  files and locations of files. We assume that files are utf8-encoded and use
  '\n' for line breaks. Specifically,

    - we convert between different representations of positions within files:
      (line,col), character offset, and byte offset. We can do this without
      opening the file by first computing the file's "shape": The length of all
      the lines and the position of all multi-byte characters within the file.

    - we convert between different representations of module locations:
      module names, paths within a source directory, absolute paths within
      temp directories, and file paths relative to the repository root. We can
      do these conversions without opening the files by first building a
      database of root directories within a repo, all source files in a repo,
      a mapping between temporary files and their coresponding modules. We
      can also guess based on longest match for cases when we don't have
      enough information.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UnicodeSyntax              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Locations where

import           ClassyPrelude             hiding (FilePath,filename,first,last)

import qualified Filesystem.Path.CurrentOS as Path

import           Control.Category.Unicode
import           Data.Char
import qualified Data.List                 as L
import           Data.Monoid.Unicode
import           Prelude.Unicode           hiding (π)

import qualified Data.ByteString.Lazy      as B
import qualified Data.IntMap               as IntMap
import qualified Data.Map                  as M
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL

import           Test.QuickCheck


-- Types ---------------------------------------------------------------------

data LineCol = LineCol Int Int
  deriving (Show,Eq)

instance Ord LineCol where
  compare (LineCol l c) (LineCol λ ξ) =
    case (compare l λ, compare c ξ) of
      (EQ,colDifference) → colDifference
      (lineDifference,_) → lineDifference

data FileShape = Shape {fsLineWidths ∷ [Int], fsUnicodeChars ∷ IntMap Int}
  deriving (Ord, Eq, Show)

type Path      = [Text]
type Extension = Maybe Text

newtype ModulePath = MP Path
newtype FilePath   = FP Path

newtype SrcPath  = Src FilePath  -- relative to a Haskell source directory.
newtype RepoPath = Repo FilePath -- relative to the root of a repository.
newtype AbsPath  = Abs FilePath  -- relative to the filesystem root.

data Span = Span{spanFile∷RepoPath, spanStart∷Int, spanLength∷Int}
type PathCol = (RepoPath, SrcPath, FileShape)
type PathDB = (Set PathCol, Map String ModulePath)


-- Utilities -----------------------------------------------------------------

lazy ∷ Text -> TL.Text
lazy = TL.fromStrict


-- Basic Instances -----------------------------------------------------------

deriving instance Eq ModulePath
deriving instance Ord ModulePath
deriving instance Show ModulePath

deriving instance Eq FilePath
deriving instance Ord FilePath
deriving instance Semigroup FilePath
deriving instance Monoid FilePath
deriving instance Show FilePath

deriving instance Eq Span
deriving instance Ord Span
deriving instance Show Span

deriving instance Eq SrcPath
deriving instance Ord SrcPath
deriving instance Show SrcPath
deriving instance Semigroup SrcPath

deriving instance Eq RepoPath
deriving instance Ord RepoPath
deriving instance Show RepoPath
deriving instance Semigroup RepoPath

deriving instance Eq AbsPath
deriving instance Ord AbsPath
deriving instance Show AbsPath
deriving instance Semigroup AbsPath


-- Path Operations -----------------------------------------------------------

parent ∷ RepoPath → Maybe RepoPath
parent (Repo(FP [])) = Nothing
parent (Repo(FP(_:p))) = Just $ Repo $ FP p

ext ∷ RepoPath → Extension
ext (Repo(FP[])) = Nothing
ext (Repo(FP(f:_))) = snd $ parseExtension f

mkMaybe ∷ Bool → a → Maybe a
mkMaybe False _ = Nothing
mkMaybe True x = Just x

validModuleComponent ∷ Text → Bool
validModuleComponent t = case T.uncons t of Nothing → False
                                            Just (c,_) → isUpper c

parseModulePath ∷ Text → ModulePath
parseModulePath = MP . reverse . T.splitOn "."

parseExtension ∷ Text → (Text,Extension)
parseExtension t = case reverse $ T.split (≡'.') t of
  [] → (t, Nothing)
  [_] → (t, Nothing)
  (ext:before) → swap (Just ext, T.intercalate "." $ reverse before)

-- TODO This doesn't handle escaping! Use a library.
parseRelativePath ∷ Text → Maybe FilePath
parseRelativePath p = case T.split (≡'/') p of
  [""]            → Just $ FP []
  ("" : _ : _)    → Nothing
  ["."]           → Just $ FP []
  ("." : q@(_:_)) → parseRelativePath $ T.intercalate "/" q
  path            → Just $ FP $ reverse path

dumpAbsPath ∷ AbsPath → Text
dumpAbsPath (Abs(FP fp)) = T.intercalate "/" $ "" : reverse fp

parseAbsPath ∷ Text → Maybe AbsPath
parseAbsPath p = case T.split (≡'/') p of
  ("" : path) → Just $ Abs $ FP $ reverse path
  _           → Nothing

srcPathMatch ∷ ModulePath → SrcPath → Bool
srcPathMatch (MP []) (Src(FP [])) = True
srcPathMatch (MP (m:mp)) (Src(FP (f:fp))) =
  m≡fst(parseExtension f) ∧ fp≡mp

moduleToSrcPath ∷ ModulePath → Extension → SrcPath
moduleToSrcPath (MP []) e = Src $ FP []
moduleToSrcPath (MP (m:mp)) e = Src $ FP $ (m <> ".hs") : mp

fileToModulePath ∷ SrcPath → ModulePath
fileToModulePath (Src(FP[])) = MP []
fileToModulePath (Src(FP(f:fp))) = MP $ fst(parseExtension f) : fp

flattenSources ∷ PathDB → Set RepoPath
flattenSources = Set.map (\(r,s,_) → srcToRepo r s) . fst

moduleToRepoFPs ∷ PathDB → ModulePath → Set RepoPath
moduleToRepoFPs (db,m) mp = flattenSources(Set.filter doesMatch db,m)
  where combine (r,s,_) = srcToRepo r s
        doesMatch (_,s,_) = srcPathMatch mp s

srcToRepo ∷ RepoPath → SrcPath → RepoPath
srcToRepo (Repo(FP prefix)) (Src(FP p)) = Repo$ FP $ p⊕prefix

srclibPath ∷ RepoPath → Text
srclibPath (Repo(FP [])) = "."
srclibPath (Repo(FP path)) = T.intercalate "/" $ reverse path


-- PathDB Operations ---------------------------------------------------------

pdbSourceFiles ∷ PathDB → Set RepoPath
pdbSourceFiles = flattenSources

matchingSourceFiles ∷ PathDB → ModulePath → Set RepoPath
matchingSourceFiles (db,x) mp = flattenSources(Set.filter f db,x)
  where f (_,s,_) = srcPathMatch mp s

--findSourceOfTmpFile ∷ PathDB → AbsPath → Set RepoPath
--findSourceOfTmpFile db p = Set.unions $ matchingSourceFiles db <$> modules
--  where tmp = pdbKnownTmpFiles db
--        modules = maybeToList $ M.lookup p tmp


-- FileShape Operations ------------------------------------------------------

firstChar ∷ LineCol
firstChar = LineCol 0 0

lastChar ∷ FileShape → LineCol
lastChar (Shape [] _) = LineCol 1 1
lastChar (Shape ws _) = LineCol (L.length ws) (L.last ws)

endOfLastLine ∷ FileShape → LineCol
endOfLastLine (Shape [] _) = LineCol 1 1
endOfLastLine (Shape ws _) = LineCol (L.length ws) (1+L.last ws)

eofPosition ∷ FileShape → LineCol
eofPosition (Shape ws _) = LineCol (1+L.length ws) 1

lineCol ∷ Int → Int → Maybe LineCol
lineCol line col | line≥1 ∧ col≥1 = Just $ LineCol line col
lineCol _    _                    = Nothing

spanFromByteOffsets ∷ (RepoPath,Int,Int) → Maybe Span
spanFromByteOffsets (fp,start,end) =
  if end<start ∨ end<0 ∨ start<0 then Nothing else
    Just $ Span fp start (end-start)

charBytes ∷ Char → Int
charBytes = TL.singleton ⋙ TL.encodeUtf8 ⋙ B.length ⋙ fromIntegral

multibyteChars ∷ TL.Text → IntMap Int
multibyteChars = TL.foldl f (0,IntMap.empty) ⋙ snd
  where f (i,m) chr | charBytes chr>1 = (i+1, IntMap.insert i (charBytes chr) m)
        f (i,m) _                     = (i+1, m)

textShape ∷ TL.Text → FileShape
textShape "" = Shape [0] IntMap.empty
textShape s = Shape ((TL.length⋙fromIntegral) <$> TL.lines s) (multibyteChars s)

fileShape ∷ Text → IO FileShape
fileShape f = (TL.pack ⋙ textShape) <$> readFile (Path.fromText f)

indexL ∷ [a] → Int → Maybe a
indexL l i = case drop i l of [] → Nothing
                              (x:_) → Just x

lineColOffset ∷ FileShape → LineCol → Maybe Int
lineColOffset shp@(Shape lineWidths _) lc@(LineCol line col) =
  let nLines = L.length lineWidths
      eof = lc ≡ eofPosition shp
  in
    if eof then Just((line-1) + sum lineWidths) else
      do width ← lineWidths `indexL` (line-1)
         guard $ (col-1) ≤ width
         return $ (col-1) + (line-1) + sum(take (line-1) lineWidths)

offsetLineCol ∷ FileShape → Int → Maybe LineCol
offsetLineCol (Shape lineWidths _) = f 1 lineWidths
  where f ∷ Int → [Int] → Int → Maybe LineCol
        f l []     off | off≡0     = Just $ LineCol l 1
                       | otherwise = Nothing
        f l (w:ws) off | off≤w     = Just $ LineCol l (1+off)
                       | otherwise = f (l+1) ws (off-w-1)

charToByteOffset ∷ FileShape → Int → Int
charToByteOffset (Shape _ mb) off = IntMap.fold (+) off mbs - IntMap.size mbs
  where mbs = fst $ IntMap.split off mb

byteToCharOffset ∷ FileShape → Int → Int
byteToCharOffset (Shape _ mb) = f 0
  where bytesAtChar i = IntMap.findWithDefault 1 i mb
        f chr remain | remain≤0 = chr
        f chr remain            = f (chr+1) (remain-bytesAtChar chr)

mkSpanSafe ∷ RepoPath → FileShape → LineCol → LineCol → Maybe Span
mkSpanSafe fn shape start end = do sChr ← lineColOffset shape start
                                   eChr ← lineColOffset shape end
                                   let toBytes = charToByteOffset shape
                                       (s,e) = (toBytes sChr, toBytes eChr)
                                       w = e-s
                                   return $ Span fn s w

showMkSpan ∷ RepoPath → ((Int,Int),(Int,Int)) → (Int,Int) → String
showMkSpan p ((l,c),(λ,ξ)) (s,e) =
  T.unpack $ T.concat $ [ "mkSpan ", srclibPath p, ":", ts l, ":", ts c
                                   , "-", ts λ, ":", ts ξ
                        , "\t", ts s, "-", ts e
                        ]
    where ts = show ⋙ T.pack

mkSpan ∷ RepoPath → FileShape → LineCol → LineCol → Maybe Span
mkSpan fn shape start@(LineCol l c) end@(LineCol λ ξ) =
  let result@(Span _ s e) = fromMaybe (Span fn 0 0) $ mkSpanSafe fn shape start end
  in trace (showMkSpan fn ((l,c),(λ,ξ)) (s,e)) $ Just result


mkSpanUnsafe ∷ RepoPath → FileShape → LineCol → LineCol → Maybe Span
mkSpanUnsafe fn shape start@(LineCol l c) end@(LineCol λ ξ) =
    traceShow shape $ trace (showMkSpan fn ((l,c),(λ,ξ)) (s,e)) result
    where result = Just $ Span fn s e
          s' = case lineColOffset shape start of
                 Just s → Just s
                 Nothing → lineColOffset shape (LineCol l 1)
          e' = case lineColOffset shape end of
                 Just e → Just e
                 Nothing → case lineColOffset shape (LineCol (λ+1) 1) of
                   Just e → Just e
                   Nothing → lineColOffset shape (LineCol λ 1)
          (s,e) = case (s',e') of
                    (Just a, Just b) → (a,b)
                    (Just a, Nothing) → (a,a)
                    (Nothing, Just b) → (b,b)
                    (Nothing, Nothing) → (0,0)


-- Arbitrary Instances -------------------------------------------------------

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary FilePath where
  arbitrary = genFP
    where genFP = do attempt ← arbitrary
                     if validPath (FP attempt) then return (FP attempt)
                                              else genFP

deriving instance Arbitrary AbsPath
deriving instance Arbitrary RepoPath
deriving instance Arbitrary SrcPath


-- Tests ---------------------------------------------------------------------

example ∷ Text
example = unlines ["→a≡s≫ ⋘ ≫df←", "", "", "" ,"", "asγdf", "", "", "αaβs⇒d⇐f\0", ""]

dropBytes ∷ Int → TL.Text → Maybe TL.Text
dropBytes n = TL.encodeUtf8 ⋙ B.drop(fromIntegral n) ⋙ TL.decodeUtf8' ⋙ toMaybe
  where toMaybe (Left _) = Nothing
        toMaybe (Right a) = Just a

dropChars ∷ Int → TL.Text → TL.Text
dropChars n = TL.drop (fromIntegral n)

convertOffset ∷ FileShape → Int → Int
convertOffset s = charToByteOffset s ⋙ byteToCharOffset s

prop_equivalentDrops ∷ Text → Int → Property
prop_equivalentDrops txt number = okOffset ==> d1≡d2
  where okOffset = offset≥0 ∧ offset≤T.length txt
        offset = number `mod` (1 + T.length txt)
        shape = textShape $ lazy txt
        d1 = Just $ dropChars offset $ lazy txt
        d2 = dropBytes (charToByteOffset shape offset) $ lazy txt

prop_convertableOffsets ∷ Text → Int → Property
prop_convertableOffsets txt number = okOffset ==> offset≡o
  where okOffset = offset≥0 ∧ offset≤T.length txt
        offset = number `mod` (1 + T.length txt)
        o = convertOffset (textShape $ lazy txt) offset

prop_convertableIndexes ∷ Text → Bool
prop_convertableIndexes txt = all ok $ allOffsets $ lazy txt
  where ok off = Just off ≡ (offsetLineCol shp off >>= lineColOffset shp)
        shp = textShape $ lazy txt

--okOffset ==> Just offset≡converted
--  where okOffset = offset≥0 ∧ offset≤T.length txt
--        offset = number `mod` (1 + T.length txt)
--        shape = textShape $ lazy txt
--        converted = offsetLineCol shape offset >>= lineColOffset shape

prop_consistentIndexOrdering ∷ Text → Int → Int → Property
prop_consistentIndexOrdering txt a b =
  okOffset a ∧ okOffset b ==> Just(compare a b) ≡ (compare <$> a' <*> b')
    where okOffset x = offset x≥0 ∧ offset x≤T.length txt
          offset x = x `mod` (1 + T.length txt)
          shape = textShape $ lazy txt
          a' = offsetLineCol shape a
          b' = offsetLineCol shape b

validPath ∷ FilePath → Bool
validPath (FP path) = all valid path
  where noSlash = T.any (≡'/') ⋙ not
        nonEmpty = T.length ⋙ (≠0)
        valid p = noSlash p ∧ nonEmpty p

prop_serializablePath ∷ RepoPath → Property
prop_serializablePath rp@(Repo fp) =
  validPath fp ==> Just fp ≡ parseRelativePath(srclibPath rp)

prop_serializableAbsPath ∷ AbsPath → Property
prop_serializableAbsPath ap@(Abs fp) =
  validPath fp ==> Just ap ≡ parseAbsPath(dumpAbsPath ap)

edgeCases ∷ TL.Text → (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
edgeCases s = (first, last, eol, eof)
  where
    shape = textShape s
    off = lineColOffset
    first = off (textShape s)   firstChar
    last  = off (textShape s) $ lastChar shape
    eol   = off (textShape s) $ endOfLastLine shape
    eof   = off (textShape s) $ eofPosition shape

-- TODO These don't work yet!
prop_lineColEdgeCases ∷ Text → Bool
prop_lineColEdgeCases s = first ∧ last ∧ eol ∧ eof
  where
    shape = textShape $ lazy s
    off = lineColOffset
    first = Just 0             ≡ off (textShape $ lazy s)  firstChar
    last  = Just(0+T.length s) ≡ off (textShape $ lazy s) (lastChar shape)
    eol   = Just(1+T.length s) ≡ off (textShape $ lazy s) (endOfLastLine shape)
    eof   = Just(2+T.length s) ≡ off (textShape $ lazy s) (eofPosition shape)

allOffsets ∷ TL.Text → [Int]
allOffsets t = [0..(fromIntegral $ TL.length t)]

allLinesCols ∷ TL.Text → [LineCol]
allLinesCols t = catMaybes $ offsetLineCol (textShape t) <$> allOffsets t

test ∷ IO ()
test = do
  let manyChecks = quickCheckWith stdArgs{maxSuccess=5000}
  quickCheck   prop_convertableIndexes
  quickCheck   prop_convertableOffsets
  quickCheck   prop_equivalentDrops
  quickCheck $ prop_convertableOffsets example
  quickCheck $ prop_equivalentDrops example
  manyChecks   prop_convertableIndexes
  -- quickCheck $ prop_convertableIndexes example
  quickCheck   prop_serializablePath
  quickCheck   prop_serializableAbsPath

  putStrLn "==== KNOWN BAD ===="
  quickCheck   prop_lineColEdgeCases
  quickCheck   prop_consistentIndexOrdering
  quickCheck $ prop_consistentIndexOrdering example
