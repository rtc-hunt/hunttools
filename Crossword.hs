module Crossword (buildDict, stringToQuery, doCrossword, QueryPart (Literal, Glob, Dot ) )
where

import Data.DAWG.Packed
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Ord

data QueryPart = Literal String | Glob | Dot | Charset [Char] deriving (Show)

rootedWords w = zipWith ((++) . (++"|")) (tails w) $ map reverse $ inits w

buildDict :: [String] -> Node
buildDict wordList = fromList $ concatMap rootedWords wordList

doQuery dict [] aPrefix = if member "" dict then [aPrefix] else []
doQuery dict (Literal qPrefix:rest) aPrefix =
	do node <- maybeToList $ lookupPrefix qPrefix dict
	   doQuery node rest $ aPrefix++qPrefix
doQuery dict (Charset qSet:rest) aPrefix =
	do char <- qSet
	   node <- maybeToList $ lookupPrefix [char] dict
	   doQuery node rest $ aPrefix++[char]
doQuery dict (Dot:rest) aPrefix =
	do child <- children dict
	   doQuery child rest $ aPrefix++[char child]
doQuery dict (Glob:rest) aPrefix =
	(doQuery dict rest aPrefix)
	  ++ (do child <- children dict
	         doQuery child (Glob:rest) $ aPrefix ++ [char child])

findLongestLiterals q = snd $ maximumBy (comparing $ length.litString.fst) $ filter (isLiteral . fst) $ zip q [0..]
	where litString (Literal a) = a
	      isLiteral (Literal _) = True
	      isLiteral _ = False

reverseQuery q = map reverseLiterals $ reverse q
	where reverseLiterals (Literal a) = Literal $ reverse a
	      reverseLiterals q = q

optimizeInitialLiteral q = let idx = findLongestLiterals q in (drop idx q) ++ (reverseQuery $ take idx q)

coalesceLiterals (Literal str1:acc) (Literal str2) = (Literal $ str1++str2):acc
coalesceLiterals (Glob:acc) Glob = Glob:acc
coalesceLiterals acc term = term:acc

tokenToQueryPart '?' = Dot
tokenToQueryPart '*' = Glob
tokenToQueryPart a = Literal [a]

stringToQuery word = optimizeInitialLiteral $ reverse $ 
		foldl coalesceLiterals [] $ 
			map tokenToQueryPart $ word ++ "|"

unoptimizeResult r = let idx = fromJust $ elemIndex '|' r in (reverse $ drop (idx+1) r) ++ (take idx r)

doQueryResult dict query = map unoptimizeResult $ doQuery dict query ""

doCrossword dict word = map unoptimizeResult $ doQuery dict (stringToQuery word) ""
