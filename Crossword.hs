{-# LANGUAGE Safe, FlexibleInstances #-}
{-|
 Module: Crossword
 Description: In-order regex-like dictionary searches.

 Core regex-like dictionary search; we implement a simple glob-like pattern, and nondeterministic retrieval from a trie structure to minimize cost.

 * Examples
 
 >>> crossword onelook "j???l??p??a?d"
 ["jeanlucpicard", ...]

-}

module Crossword ( 
  CrosswordQuery,
  --finalizeQuery,
  --finalizeQueryStraight,
  -- * Entry points
  crossword,
  crosswordSubseq,
  -- * Programmatic interface
  QueryPart (Literal, Glob, Dot, Charset, Star, Opt ),
  -- * Dictionaries
  CrosswordDictionary (BidirectionalDictionary, ForwardDictionary),
  buildDict,
  buildDictAscSplit,
  buildDictUnidir,
  getCrosswordDAWG
  )
where

import Data.DAWG.Packed64
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Ord
import Data.Char
import MergeDawg

data QueryPart = 
  -- | A literal string of characters. This could equivalently be written with as a Literal Char type, but we chose to simplify the QueryPart level of the representation.
  Literal String 
  -- | Matches zero-or-more arbitrary characters. Notably, this is __not__ the kleene star; in regex terms it's .*. * is parsed to Glob.
  | Glob 
  -- | Exactly one arbitrary character. Named after regex dot, but we parse ? to it following shell and crossword-solver conventions.
  | Dot 
  -- | Character sets; matches exactly one of any of the given characters. Not available from the string-to-query parser.
  | Charset [Char] 
  -- | Kleene star. Not available from the string-to-query parser.
  | Star QueryPart 
  -- | FIXME: what does this do again?
  | Opt String
  deriving (Show)

-- | A crossword dictionary.
data CrosswordDictionary = 
  -- | A dictionary that has been munged to allow starting in the middle of the word; see 'optimizeInitialLiteral' in the source for details.
  BidirectionalDictionary Node
  -- | A dictionary that is a simple trie of the words in the dictionary.
  | ForwardDictionary Node

getCrosswordDAWG (BidirectionalDictionary a) = a
getCrosswordDAWG (ForwardDictionary a) = a

rootedWords :: String -> [String]
rootedWords w = let res = zipWith ((++) . (++"|")) (tails w) $ map reverse $ inits w in (foldl' (const id) "" res ::String) `seq` res

buildDict :: [String] -> Node
buildDict wordList = fromList $ let res=concatMap rootedWords wordList in foldl' (const id) "" res `seq` res

buildDictUnidir wordList = fromAscList wordList

buildDictAscSplit wordList = buildHugeDAWG $ wordList
--	do wd <- wordList
--	   id $! rootedWords wd

--concatMap rootedWords wordList

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
doQuery dict (Star (Charset s):rest) aPrefix =
	(doQuery dict rest aPrefix)
          ++ (do char <- s
	         node <- maybeToList $ lookupPrefix [char] dict
		 doQuery node (Star (Charset s):rest) $ aPrefix++[char])
doQuery dict ((Opt str):rest) aPrefix =
        (do node <- maybeToList $ lookupPrefix str dict
            doQuery node rest $ aPrefix++str) ++ 
        (doQuery dict rest aPrefix)

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

-- | A typeclass of types we can use as a crossword query.
-- 
-- finalizeQuery and finalizeQueryStraight are probably going to merge into a single getQueryParts function. The distinction now is that finalizeQuery produces a query appropriate for a 'BidirectionalDictionary', while finalizeQueryStraight produces a query appropriate for a 'ForwardDictionary'.
class CrosswordQuery a where
  finalizeQuery :: a -> [QueryPart]
  finalizeQueryStraight :: a -> [QueryPart]

instance CrosswordQuery [Char] where
  finalizeQuery = stringToCrosswordQuery . (map toLower)
  finalizeQueryStraight = stringToStraightCrosswordQuery . (map toLower)

instance CrosswordQuery [QueryPart] where
  finalizeQuery a = optimizeInitialLiteral $ finalizeQueryStraight $ if any isLiteralWithPipe a then a else a++[Literal "|"]
        where isLiteralWithPipe (Literal str) = '|' `elem` str
              isLiteralWithPipe _ = False
  finalizeQueryStraight = foldl coalesceLiterals []

stringToCrosswordQuery word = optimizeInitialLiteral $ stringToStraightCrosswordQuery $ word ++ "|"

stringToStraightCrosswordQuery word = reverse $
	foldl coalesceLiterals [] $
		map tokenToQueryPart $ word

unoptimizeResult r = let idx = fromJust $ elemIndex '|' r in (reverse $ drop (idx+1) r) ++ (take idx r)

doQueryResult dict query = map unoptimizeResult $ doQuery dict query ""


-- | Run the given query against a crossword dictionary. This is the primary entry point for this module.
crossword :: (CrosswordQuery a) => CrosswordDictionary -> a -> [String]
crossword (BidirectionalDictionary dict) word = map unoptimizeResult $ doQuery dict (finalizeQuery word) ""
crossword (ForwardDictionary dict) word = doQuery dict (finalizeQueryStraight word) ""

-- | Treat the input as a raw string, and query for subsequences of that string in the dictionary. Does not have metacharacters, and essentially wraps every letter in an individual 'Opt'.
crosswordSubseq (BidirectionalDictionary dict) word = map unoptimizeResult $ doQuery dict (Literal "|":(map (Opt . (\a->[a]) . toLower) word)) ""
crosswordSubseq (ForwardDictionary dict) word = doQuery dict (reverse $ map (Opt . (\a->[a]) . toLower) word) ""
