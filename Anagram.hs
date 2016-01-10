{-# LANGUAGE OverloadedStrings #-}

module Anagram (doAnagram, buildAnagramDictionary, getHistogram, AnagramDictionary, doMultiAnagram) where

import qualified Data.Trie as T
import qualified Data.Trie.Convenience as TC
import Data.List
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as BSW
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

symbols = ['a'..'z']

newtype Histogram = Histogram BS.ByteString
newtype AnagramDictionary = AnagramDictionary (T.Trie [[Char]])
fromAnagramDictionary (AnagramDictionary trie) = trie
instance Show Histogram where
	show (Histogram bs) = "fromPairs " ++ (show $ zip symbols $ BS.unpack bs)

getHistogram str = Histogram $ BS.pack $ let q=M.fromListWith (+) [(c,1)|c<-str] in Prelude.map (\s -> M.findWithDefault 0 s q) symbols

fromHistogram (Histogram h) = h

subHistogram (Histogram h) (Histogram r) = Histogram $ BS.pack $ BS.zipWith (-) h r

buildAnagramDictionary bigString = AnagramDictionary $ TC.fromListWith (++) $ map (\a->(fromHistogram $ getHistogram a, [a])) $ words bigString

clamp a = if a>127 then 0 else a
decrem allowed used = allowed - (clamp used)
queryAnagramDictionary trie (mi, ma) hist = 
	concat [
	  recQuery mii ma (BS.unpack $ fromHistogram hist) trie
	  | mii <- [0..mi] ]
	where recQuery mini maxi (a:[]) tr = if mini<=a then concat $ maybeToList $ T.lookup (BS.pack [a-mini]) tr else []
	      recQuery mini maxi (a:r) tr = concat
	      	[ recQuery (decrem mini (a-v)) (decrem maxi (v-a)) r $ T.lookupBy (const id) (BS.pack [v]) tr | v<-[(clamp $ a-mini)..a+maxi] ]

lookupHistogram trie hist = queryAnagramDictionary trie (0,0) hist

doAnagram dict str =
	queryAnagramDictionary (fromAnagramDictionary dict) (fromIntegral $ (length str) `quot` 2, (fromIntegral $ length $ filter (=='?') str)) $ getHistogram str

doMultiAnagram dict str =
	queryAnagramDictionary (fromAnagramDictionary dict) (fromIntegral $ (length str), (fromIntegral $ length $ filter (=='?') str)) $ getHistogram str

