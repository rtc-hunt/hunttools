module Main where
import Crossword
import Anagram
import System.Environment
import Data.DAWG.Packed64
import Data.Char

main = do
	cliArgs <- getArgs
	file <- {-# SCC readFile #-} readFile $ cliArgs!!0
	let dawg = buildDictUnidir $ {-# SCC lines_file #-} lines $ map toLower $ file
	toFile (cliArgs!!1) dawg
	putStrLn "Crossword DAWG written."
	file <- readFile $ cliArgs!!0
	let dawg2 = fromAnagramDictionary $  buildAnagramDictionary $ lines $ map toLower $ file
	toFile (cliArgs!!2) dawg2
	putStrLn "Anagram DAWG written. Exiting."

