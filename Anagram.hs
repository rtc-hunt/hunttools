{-# LANGUAGE OverloadedStrings, FlexibleInstances, Safe #-}

module Anagram (anagram, buildAnagramDictionary, getHistogram, AnagramDictionary (AnagramDictionary), anagramAny, anagramMin, anagramFull, subHistogram, fromAnagramDictionary, anagramExact, Histogram (Histogram), seqAna) where

import qualified Data.Map as M
import Data.Maybe
import Data.DAWG.Packed64
import Data.Char
import Data.List

symbols = ['a'..'z']

data Histogram = Histogram { blankCount :: Int, fromHistogram :: [Int] }
histogramToString (Histogram blanks bs) = map toEnum bs

charCount (Histogram blanks hist) = blanks + (foldl ((+)::Int->Int->Int) 0 hist)

newtype AnagramDictionary = AnagramDictionary Node
fromAnagramDictionary (AnagramDictionary trie) = trie

instance Show Histogram where
  show (Histogram blanks bs) = "histogramFromPairs " ++ (show blanks) ++ " " ++ (show $ zip symbols $ map fromEnum bs)

class (Anagrammable a) where
  getHistogram :: a->Histogram

instance (Anagrammable [Char]) where
  getHistogram str = 
    Histogram (length $ filter (=='?') str) $ let
       q=M.fromListWith (+) [(toLower c,1)|c<-str] 
    in Prelude.map (\s -> M.findWithDefault 0 s q) symbols

instance (Anagrammable Histogram) where
  getHistogram hist = hist

--fromHistogram (Histogram _ h) = h

subHistogram h r = 
    let consumedBlanks = sum $ zipWith (\a b->clamp $ a - b) (fromHistogram r) (fromHistogram h)
     in Histogram (blankCount h-blankCount r-consumedBlanks) $ zipWith (-) (fromHistogram h) (fromHistogram r)
  where clamp a = if a<0 then 0 else a

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

anagram :: (Anagrammable a) => AnagramDictionary -> a -> [String]
anagram dict str =
  queryAnagramDictionary (fromAnagramDictionary dict) ((charCount hist) `quot` 2) hist
  where hist = getHistogram str

anagramAny :: (Anagrammable a) => AnagramDictionary -> a -> [String]
anagramAny dict str =
  queryAnagramDictionary (fromAnagramDictionary dict) (charCount hist) hist
  where hist = getHistogram str

anagramMin :: (Anagrammable a) => AnagramDictionary -> a -> Int -> [String]
anagramMin dict str min =
  queryAnagramDictionary (fromAnagramDictionary dict) ((charCount hist)-min) hist
  where hist = getHistogram str

anagramFull :: (Anagrammable a) => AnagramDictionary -> a -> [String]
anagramFull dict str = queryAnagramDictionary (fromAnagramDictionary dict) 0 hist
  where hist = getHistogram str

anagramExact :: (Anagrammable a) => AnagramDictionary -> Int -> a -> [String]
anagramExact dict k str = queryAnagramDictionaryK (fromAnagramDictionary dict) ((charCount hist) - k) $ hist
  where hist = getHistogram str

accblat :: AnagramDictionary -> [([String], Histogram)]->Int->[([String], Histogram)]
accblat dict acc k=do
  (seq, histo) <- acc
  next <- anagramExact dict k histo
  return (seq++[next], histo `subHistogram` getHistogram next)

seqAna :: AnagramDictionary -> String -> [Int] -> [[String]]
seqAna dict str ints =map fst $ foldl (accblat dict) [([], getHistogram str)] ints

