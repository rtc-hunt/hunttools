module Main where
import Crossword
import System.Environment
import Data.DAWG.Packed
main = do
	cliArgs <- getArgs
	file <- {-# SCC readFile #-} readFile $ cliArgs!!0
	let dawg = buildDict $ {-# SCC lines_file #-} lines file
	toFile (cliArgs!!1) dawg

