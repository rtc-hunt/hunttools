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


main = do
	(destFile:srcFiles) <- getArgs
	dawgs <- mapM fromFile srcFiles
	putStrLn $ "Merging DAWGs to "++ destFile ++ " ..."
	dawg <- mergeToVector dawgs
	toFile destFile dawg
	putStrLn "Done."


	
