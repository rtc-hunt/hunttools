module MergeDawg (mergeDAWG, buildHugeDAWG) where

import Data.DAWG.Packed
import Data.Word
import Data.Maybe
import Data.Ord
import Data.List.Split
import Data.List
import qualified Data.Vector.Unboxed as V
import Debug.Trace

maybesMerge cmp xs [] = zip (map Just xs) (repeat Nothing)
maybesMerge cmp [] ys = zip (repeat Nothing) $ map Just ys
maybesMerge cmp xs@(x:xt) ys@(y:yt) =
	case cmp x y of
	LT -> (Just x, Nothing):maybesMerge cmp xt ys
	EQ -> (Just x, Just y)      :maybesMerge cmp xt yt
	GT -> (Nothing, Just y):maybesMerge cmp xs yt

mergeDawgAssoc :: Maybe Node -> Maybe Node -> Int -> [Word32] -> (Word32, Int, [Word32])

mergeDawgAssoc a b startingIdx topSavedList =
	let mergedChildren = maybesMerge (comparing char) (maybe [] children a) (maybe [] children b)
	    (childNodes, newIdx, newSavedList) = foldl 
	      (\(cs, curIdx, savedList) child->
	        let (th, nIdx, sList) = mergeDawgAssoc (fst child) (snd child) curIdx savedList
		 in (th:cs, nIdx, sList))
	      ([], startingIdx, topSavedList) $ mergedChildren
	    in
	    (pack (maybe (char $ fromJust b) char a) False (maybe False endOfWord a || maybe False endOfWord b) $ if childNodes == [] then 0 else newIdx,
	     newIdx+(length childNodes),
	     if childNodes == [] then newSavedList else
	       (head childNodes+2):(tail childNodes) ++ newSavedList)

mergeDAWG :: Node -> Node -> Node
mergeDAWG a b = 
         let (root, curIdx, savedList) = mergeDawgAssoc (Just a) (Just b) 1 [0]
	     vec = V.reverse $ V.fromList ((root+2):savedList)
	  in unpack (V.unsafeLast vec) vec

buildHugeDAWG :: [String] -> Node
buildHugeDAWG input = foldl' (\accum i -> trace ((show $ V.length $ nodeVector $ accum) ++ " - Tick\n") $ mergeDAWG (fromList i) accum) (fromList []) $ chunksOf 500000 input





