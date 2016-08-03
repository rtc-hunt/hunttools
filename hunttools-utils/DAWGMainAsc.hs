{-# LANGUAGE Safe #-}
module Main where
import Crossword
import System.Environment
import Data.DAWG.Packed64
main = do
	cliArgs <- getArgs
	file <- {-# SCC readFile #-} readFile $ cliArgs!!0
	let dawg = buildDictAscSplit $ {-# SCC lines_file #-} lines file
	toFile (cliArgs!!1) dawg

