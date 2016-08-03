module Main where
import Crossword
import Anagram
import MergeDawg
import System.Environment
import Data.DAWG.Packed64
import Data.Char
import System.IO
import Data.List.Split
import Control.Concurrent
import Control.Concurrent.ParallelIO
import Data.IORef
import Data.Vector.Binary
import Data.Binary
--import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as V

writeSub ctr words = do
	(tempName, tempHandle) <- openTempFile "/tmp/HugeDawg/" "tempSubWords.txt"
	hPutStr tempHandle $ unlines words
	hSeek tempHandle AbsoluteSeek 0
	hSetEncoding tempHandle utf8
	cur<-atomicModifyIORef' ctr (\a->(a+1, a))
	putStrLn $ "Subset " ++ show cur ++ " written."
	return tempHandle

subDawgs :: Int -> IORef Int -> Handle -> IO (FilePath, FilePath)
subDawgs nSubs ctr tempHandle = do
	wordsRaw <- hGetContents tempHandle
	let words = lines wordsRaw
	(crossTempName, crossTempHandle) <- openTempFile "/tmp/HugeDawg/" "cross.dict"
	(anaTempName, anaTempHandle) <- openTempFile "/tmp/HugeDawg/" "anagram.dict"
	let crossDawg = buildDict words
	let anaDawg = fromAnagramDictionary $ buildAnagramDictionary words
	hClose crossTempHandle
	hClose anaTempHandle
	toFile crossTempName crossDawg
	toFile anaTempName anaDawg
	cur<-atomicModifyIORef' ctr (\a->(a+1, a))
	putStrLn $ "Sub-DAWG " ++ show cur ++ " of " ++ show nSubs ++ "written"
	return (crossTempName, anaTempName)

main = do
	(srcFile:crossFile:anaFile:cliArgs) <- getArgs
	putStrLn $ "Splitting file into chunks: " ++ srcFile
	hndl <- openFile srcFile ReadMode
	hSetEncoding hndl utf8
	ctr <- newIORef (0::Int)
	src <- hGetContents hndl
	tempSplit <- mapM (writeSub ctr) $ chunksOf 50000 $ lines $ map toLower src
	putStrLn "Spawning initial dawg threads..."
	chunkCount <- readIORef ctr
	writeIORef ctr 0
	filePairs <- parallel $ map (subDawgs chunkCount ctr) tempSplit
	let crossDicts = map fst filePairs
	let anaDicts = map snd filePairs

	stopGlobalPool
	
	crossDawgs <- mapM fromFile crossDicts
	putStrLn "Merging for crossword..."
	crossDawg <- mergeToVector crossDawgs
	toFile crossFile crossDawg
	
	anaDawgs <- mapM fromFile anaDicts
	putStrLn "Merging for anagram..."
	anaDawg <- mergeToVector anaDawgs
	toFile anaFile anaDawg

--	anaOut<-openFile anaFile WriteMode
--	anaDawgs <- mapM fromFile anaDicts
--	putStrLn "Merging for anagram..."
--	mergeToHandleComp anaOut anaDawgs
	putStrLn "Done."


	
