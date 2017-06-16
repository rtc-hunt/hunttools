{-# LANGUAGE OverloadedStrings, FlexibleInstances, Safe #-}
{-|
Module : Anagram
Description : Dictionary structure and queries for anagrams.

Core anagram toolkit. The main entry points are anagram, anagramAny, anagramMin, anagramFull, and anagramExact. Histograms are the internal form for anagram queries, and are a count of how many of each letter is present in the query.

* Examples

>>> anagram ukacd "eoynkm"
["monkey"]

-}
module Anagram (anagram, buildAnagramDictionary, getHistogram, AnagramDictionary (AnagramDictionary), anagramAny, anagramMin, anagramFull, subHistogram, fromAnagramDictionary, anagramExact, Histogram (Histogram), seqAna) where

import qualified Data.Map as M
import Data.Maybe
import Data.DAWG.Packed64
import Data.Char
import Data.List
import Data.Ord

-- | The list of symbols we support anagramming; anything not in this list is simply dropped. We use a finite list so 'Histogram' and 'AnagramDictionary' don't need to store actual characters.
symbols = ['a'..'z']

-- | The Histogram type; we implement anagram queries by converting the string to one of these, and querying the database for matches.
data Histogram = Histogram {
  -- | We differentiate internally between real letters and wildcards; the underlying code has to deal with them fully differently.
  blankCount :: Int
  -- | the list of counts; the order is the same order as 'symbols'.
  ,fromHistogram :: [Int]
  }

-- | Encode a histogram's counts as a string of chars. /not/ a string of the letters to anagram.
histogramToString (Histogram blanks bs) = map toEnum bs

charCount (Histogram blanks hist) = blanks + (foldl ((+)::Int->Int->Int) 0 hist)

-- | A dictionary structure for querying anagrams.
-- 
-- Internally, this is just the same sort of trie structure as 'Crossword.CrosswordDictionary', but instead of storing actual characters we treat the members as 'Word8's of letter counts, followed by the actual word. This allows us to eliminate common tails among the given words essentially for free.
newtype AnagramDictionary = AnagramDictionary Node
-- | Get the raw trie from an AnagramDictionary.
fromAnagramDictionary (AnagramDictionary trie) = trie

instance Show Histogram where
  show (Histogram blanks bs) = "histogramFromPairs " ++ (show blanks) ++ " " ++ (show $ zip symbols $ map fromEnum bs)

-- | Build a histogram manually from a number of blanks and a list of character, count pairs.
histogramFromPairs k lst = Histogram k $ snd <$> (filter ((`elem` symbols) . fst) $ unionBy (\a b-> fst a == fst b) (sortBy (comparing fst) lst) $ zip symbols $ repeat 0 )

-- | A typeclass for things that can be treated as anagram queries.
class (Anagrammable a) where
  -- | Get the query in the form of a Histogram, to actually use as a query.
  getHistogram :: a->Histogram

instance (Anagrammable [Char]) where
  -- | If given a string, convert it to a Histogram.
  getHistogram str = 
    Histogram (length $ filter (=='?') str) $ let
       q=M.fromListWith (+) [(toLower c,1)|c<-str] 
    in Prelude.map (\s -> M.findWithDefault 0 s q) symbols

instance (Anagrammable Histogram) where
  -- | getHistogram = id when the type is already a Histogram.
  getHistogram hist = hist

--fromHistogram (Histogram _ h) = h

-- | Subtract a histogram from a histogram. "aabc" - "ab" = "ac"
subHistogram h r = 
    let consumedBlanks = sum $ zipWith (\a b->clamp $ a - b) (fromHistogram r) (fromHistogram h)
     in Histogram (blankCount h-blankCount r-consumedBlanks) $ zipWith (-) (fromHistogram h) (fromHistogram r)
  where clamp a = if a<0 then 0 else a

-- | Construct an anagram dictionary from a raw list of strings. Gets very slow on large dictionaries, due to the time to construct the trie.
buildAnagramDictionary bigStringList = 
  AnagramDictionary $
    fromList $
      map (\a->(map toEnum $ fromHistogram $ getHistogram a)++"$"++a) bigStringList

queryAnagramDictionary trie mi hist = 
  do mii <- [0..mi]
     sort $ recQuery mii (blankCount hist) (fromHistogram hist) trie
  where recQuery mini maxi ([]) tr = if mini>0 || maxi>0 then [] else (maybeToList $ lookupPrefix ['$'] tr) >>= toList
        recQuery mini maxi (a:r) tr = do
          v<- [(clamp $ a-mini)..a+maxi]
          next <- maybeToList $ lookupPrefix [toEnum v] tr
          recQuery (decrem mini (a-v)) (decrem maxi (v-a)) r next
        clamp a = if a<0 then 0 else a
        decrem allowed used = allowed - (clamp used)

queryAnagramDictionaryK trie mii hist = 
  sort $ recQuery mii (blankCount hist) (fromHistogram hist) trie
  where recQuery mini maxi ([]) tr = if mini>0 || maxi>0 then [] else (maybeToList $ lookupPrefix ['$'] tr) >>= toList
        recQuery mini maxi (a:r) tr = do
          v<- [(clamp $ a-mini)..a+maxi]
          next <- maybeToList $ lookupPrefix [toEnum v] tr
          recQuery (decrem mini (a-v)) (decrem maxi (v-a)) r next
        clamp a = if a<0 then 0 else a
        decrem allowed used = allowed - (clamp used)


lookupHistogram trie hist = queryAnagramDictionary trie 0 hist

-- | Basic anagram. Constrains the set of returned anagrams to use half and all of the letters in the query.
anagram :: (Anagrammable a) => AnagramDictionary -> a -> [String]
anagram dict str =
  queryAnagramDictionary (fromAnagramDictionary dict) ((charCount hist) `quot` 2) hist
  where hist = getHistogram str

-- | As above, but from all to none of the letters in the query.
anagramAny :: (Anagrammable a) => AnagramDictionary -> a -> [String]
anagramAny dict str =
  queryAnagramDictionary (fromAnagramDictionary dict) (charCount hist) hist
  where hist = getHistogram str

-- | Anagram at-least-k letters from the query.
--
-- prop> anagram dict query = anagramMin dict query (length query `quot` 2)
--
-- prop> anagramAny dict query = anagramMin dict query 0
anagramMin :: (Anagrammable a) => AnagramDictionary -> a -> Int -> [String]
anagramMin dict str min =
  queryAnagramDictionary (fromAnagramDictionary dict) ((charCount hist)-min) hist
  where hist = getHistogram str

-- | Anagram all the letters in the query.
--
-- prop> anagramFull dict query = anagramMin dict query (length query)
anagramFull :: (Anagrammable a) => AnagramDictionary -> a -> [String]
anagramFull dict str = queryAnagramDictionary (fromAnagramDictionary dict) 0 hist
  where hist = getHistogram str

-- | Anagram exactly k letters from the query.
--
-- prop> all (\a->length a == k) (anagramExact dict k query)
anagramExact :: (Anagrammable a) => AnagramDictionary -> Int -> a -> [String]
anagramExact dict k str = queryAnagramDictionaryK (fromAnagramDictionary dict) ((charCount hist) - k) $ hist
  where hist = getHistogram str

accblat :: AnagramDictionary -> [([String], Histogram)]->Int->[([String], Histogram)]
accblat dict acc k=do
  (seq, histo) <- acc
  next <- anagramExact dict k histo
  return (seq++[next], histo `subHistogram` getHistogram next)

-- | Very simple n-fold anagram implementation.
seqAna :: AnagramDictionary -> String -> [Int] -> [[String]]
seqAna dict str ints =map fst $ foldl (accblat dict) [([], getHistogram str)] ints

