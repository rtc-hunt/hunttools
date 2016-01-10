module Main where
import Crossword
import System.Environment
import Data.DAWG.Packed
import MergeDawg
main = do
	cliArgs <- getArgs

	dawg1 <- fromFile $ cliArgs!!0
	dawg2 <- fromFile $ cliArgs!!1

	toFile (cliArgs!!2) $ mergeDAWG dawg1 dawg2


