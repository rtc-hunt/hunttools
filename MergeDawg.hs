{-# LANGUAGE Trustworthy #-}
{-|
 Module: MergeDawg
 Description: Tools for building larger DAWGs for dictionaries. Mostly needed for handling onelook-sized data.
 -}
module MergeDawg (mergeDAWG, buildHugeDAWG, mergeDawgNew, mergeToHandle, mergeToVector) where

import Data.DAWG.Packed64
import Data.Word
import Data.Maybe
import Data.Ord
import Data.List.Split
import Data.List
import Data.Bits
import Debug.Trace
import Data.Binary
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS
import Data.Function
import System.IO
import Control.Monad
--import qualified Data.Vector.Unboxed.Mutable as VM
import Data.IORef
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.Digest.CRC32 as CRC
import Control.DeepSeq
import Data.Vector.Storable.MMap
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GVM

maybesMerge cmp xs [] = zip (map Just xs) (repeat Nothing)
maybesMerge cmp [] ys = zip (repeat Nothing) $ map Just ys
maybesMerge cmp xs@(x:xt) ys@(y:yt) =
	case cmp x y of
	LT -> (Just x, Nothing):maybesMerge cmp xt ys
	EQ -> (Just x, Just y)      :maybesMerge cmp xt yt
	GT -> (Nothing, Just y):maybesMerge cmp xs yt

mergeDawgAssoc :: Maybe Node -> Maybe Node -> Int -> [Word64] -> (Word64, Int, [Word64])

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
	     vec = GV.reverse $ GV.fromList ((root+2):savedList)
	  in unpack (GV.unsafeLast vec) vec

optTrace _ _ cur = cur
foptTrace str pcnt cur@(_, cnt, _) =
	if pcnt `div` 10000 < cnt `div` 10000
	   then trace (show (cnt, str)) cur
	   else cur

sumWidget a (d, e, f) = (a++d, e, f)

foldn a b c = if null c then b else a (head c) b

type Writer = [Word64] -> IO ()

newcursiveMerge :: Writer -> ([Word64]->IO (Maybe Int)) -> (Int, [Word64]) -> [Node] -> IO (Int, [Word64])
newcursiveMerge handle getAlreadyDone (count, siblings) nodes =
	do
	  let nexts = force $ groupBy ((==) `on` char) $ reverse $ sortBy (comparing char) $ do { n <- nodes; children n }
	  let buildThis = force $ pack (char $ head nodes) False (any endOfWord nodes)
	  if null nexts
	    then return $!! (count, siblings++[buildThis 0])
	    else do
	            (newCount, childrenR) <- foldM (newcursiveMerge handle getAlreadyDone) (count, []) nexts
		    (newCount, childrenR) `deepseq` return ()
	            let children = force $ init childrenR ++ [last childrenR `setBit` 1]
	            maybeAlreadyDone <- getAlreadyDone children
		    case maybeAlreadyDone of
		      Just childPtr -> return (newCount, siblings++[buildThis childPtr])
		      Nothing -> do
		        handle children
			return $!! (newCount + length children, siblings ++ [buildThis newCount])

handleWriter hndl = mapM_ (LB.hPut hndl . encode) 

emptyGetAlreadyDone _ = return Nothing

mergeToHandle :: Handle -> [Node] -> IO ()
mergeToHandle handle roots = do
	(count, [root]) <- newcursiveMerge (handleWriter handle) emptyGetAlreadyDone (1, []) roots
	LB.hPut handle $ encode $ root `setBit` 1


{- Some space-efficient hashmap stuff -}

mapInsert :: [[Word64]->Int] -> VM.IOVector Int -> [Word64] -> Int -> IO ()

mapInsert (hash:hashes) vec elem value = do
	let idx = hash elem
	headval <- GVM.read vec idx
	if isEmptySentinel headval
	  then GVM.write vec idx value
	  else mapInsert hashes vec elem value


mapFetch (hash:hashes) isCorrectElem vec elem = do
	let idx = hash elem
	headval <- VM.read vec idx
	if isEmptySentinel headval
	  then return Nothing
	  else do
	    ice <- isCorrectElem elem headval
	    if ice
	          then do
		    --putStrLn "Found node"
		    return $ Just headval
		  else do
		    --putStrLn "Found collided"
		    mapFetch hashes isCorrectElem vec elem

isEmptySentinel = (==0)
hashList :: [[Word64] -> Int]
hashList = map ((fromIntegral . (`shiftR` 4) . (decode . LB.fromStrict . BS.take 32 :: BS.ByteString->Word32)) .)  $ unfoldr (\b -> Just (LB.toStrict . encode . CRC.crc32 . b, LB.toStrict . encode . CRC.crc32 . b) ) (BS.concat . map (LB.toStrict . encode))

ice :: IORef (VM.IOVector Word64) -> [Word64] -> Int -> IO Bool
ice vec elem idx = do
	vv <- readIORef vec
	sub <- (GV.freeze (VM.slice idx (length elem) vv) :: IO (VS.Vector Word64))
	return $ GV.and $ GV.take (length elem) $ GV.zipWith (==) sub $ GV.fromList elem


vecToNode a = unpack (GV.unsafeLast a) a

mergeToVector roots = do
	nv <- unsafeMMapMVector "TempVector" ReadWrite Nothing
--	nv <- VM.new 200000000
	vec <- newIORef nv
	hashMap <- VM.new $ 2^28
	cursor <- newIORef (1::Int)
	let putSingle a = do
		crsr<-readIORef cursor
		vv <- readIORef vec
	        VM.write vv crsr a
		modifyIORef cursor (1+)
		unless ((crsr `mod` 1000) /= 0) $ putStrLn $ "Nodes written: " ++ show crsr
	let writer as = do
		crsr <- readIORef cursor
		vv <- readIORef vec
		unless (crsr < VM.length vv) $ do
			newVec <- VM.grow vv 1000000
			writeIORef vec newVec
		mapM_ putSingle as
		mapInsert hashList hashMap as crsr
	let getDone = mapFetch hashList (ice vec) hashMap
	(count, [root]) <- newcursiveMerge writer getDone (1, []) roots
	writer [root `setBit` 1]
	vv <- readIORef vec
	fv <- GV.unsafeFreeze $ VM.slice 0 (count+1) vv :: IO (VS.Vector Word64)
	return $ vecToNode $ GV.convert fv

--recursiveMerge :: String -> [Node] -> ([Word64], Int, [Word64]) -> ([Word64], Int, [Word64])
recursiveMerge strCur (result, cnt, siblings) nodes =
	let nexts = groupBy ((==) `on` char) $ sortBy (comparing char) $ do { n <- nodes; children n } -- Write out children's children
	    (result2, cnt2, childs) = seq nexts $ foldl' (recursiveMerge (strCur++[char $ head nodes])) (result, cnt, []) nexts
	 in optTrace (strCur++[char $ head nodes]) cnt 
	     (result2 ++ (if null childs then [] else init childs++[last childs `setBit` 1]),
	      cnt2 + length childs,
	      siblings ++ [pack (char $ head nodes) False (any endOfWord nodes) (if null childs then 0 else cnt2)] )

mergeDawgNew roots =
	let (dawg, size, [root]) = recursiveMerge "" ([],1,[]) roots
	 in --LB.concat $ map encode $
	   [0] ++ dawg -- ++[root]




buildHugeDAWG :: [String] -> Node
buildHugeDAWG input = foldl' (\accum i -> trace ((show $ GV.length $ nodeVector $ accum) ++ " - Tick\n") $ mergeDAWG (fromList i) accum) (fromList []) $ chunksOf 500000 input





