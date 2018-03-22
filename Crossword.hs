{-# LANGUAGE Safe, FlexibleInstances #-}
{-|
 Module: Crossword
 Description: In-order regex-like dictionary searches.

 Core regex-like dictionary search; we implement a simple glob-like pattern, and nondeterministic retrieval from a trie structure to minimize cost.

 Using strings, we have two metacharacters available: "*" and "?".
 
 * "*" refers to arbitrarily many (including zero) arbitrary characters; it functions like * in the shell. Worth note is that this is /not/ regex behavior, where it refers to arbitrarily many /of the pattern to it's left/.

 * "?" refers to exactly one arbitrary character. This matches most shells' use of "?", and corresponds to regex "."

 = Examples
 
 >>> crossword onelook "j???l??p??a?d"
 ["jeanlucpicard", ...]

 >>> crossword onelook "je*lu?p*d"
 ["jeanlucpicard"]

 >>> crossword onelook [Charset "jh",Charset "ea",Charset "ar",Charset "nr",Charset "ly",Charset "up",Charset "co",Charset "pt",Charset "it",Charset "ce",Charset "ar", Star (Charset "dr")]
 ["harrypotter", "jeanlucpicard"]

 >>> sortBy (comparing $ negate . length) $ crosswordSubseq "jheaarnrlyupcoptitcearrd"
 ["jeanlucpicard", ... "harrypotter", ...]
 
 (Sorting just to get the longest results first.)
-}

module Crossword ( 
  CrosswordQuery,
  -- * Entry points
  crossword,
  crosswordSubseq,
  crosswordSuperseqN,
  -- * Programmatic interface
  QueryPart (Literal, Glob, Dot, Charset, Star, Opt ),
  convertQuery,
  -- * Utilities
  splitNTimes,
  insertNCopies,
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
import Control.Monad.Loops
import MergeDawg

data QueryPart = 
  -- | A literal string of characters. This could equivalently be written with as a Literal Char type, but we chose to simplify the QueryPart level of the representation.
  Literal String 
  -- | Matches zero-or-more arbitrary characters. * is parsed to this term. Notably, this is __not__ the kleene star; it's the shell *, equivalent to regex ".*"
  | Glob 
  -- | Exactly one arbitrary character. Named after regex dot, but we use ? for it following shell and crossword-solver conventions.
  | Dot 
  -- | Character sets; matches exactly one of any of the given characters. Not available via 'convertQuery' applied to a String.
  | Charset [Char] 
  -- | Kleene star. Not available via 'convertQuery' applied to a String.
  | Star QueryPart 
  -- | Matches zero or one copies of the entirety of the given string. Not available via 'convertQuery' applied to a String.
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

-- use with reverse . foldl coalesceLiterals []
coalesceLiterals (Literal str1:acc) (Literal str2) = (Literal $ str1++str2):acc
coalesceLiterals (Glob:acc) Glob = Glob:acc
coalesceLiterals acc term = term:acc

tokenToQueryPart '?' = Dot
tokenToQueryPart '*' = Glob
tokenToQueryPart a = Literal [toLower a]

-- | A typeclass of things we can use as a crossword query.
class CrosswordQuery a where
  -- | Convert an a to the native "list of QueryParts" format.
  convertQuery :: a -> [QueryPart]

instance CrosswordQuery [Char] where
  convertQuery = reverse . foldl coalesceLiterals [] . map tokenToQueryPart

instance CrosswordQuery [QueryPart] where
  convertQuery = id

toBidirect a = optimizeInitialLiteral $ reverse $ foldl coalesceLiterals [] $ if any isLiteralWithPipe a then a else a++[Literal "|"]
  where isLiteralWithPipe (Literal str) = '|' `elem` str
        isLiteralWithPipe _ = False

fromBidirect r = let idx = fromJust $ elemIndex '|' r in (reverse $ drop (idx+1) r) ++ (take idx r)

-- | Run the given query against a crossword dictionary. This is the primary entry point for this module.
crossword :: (CrosswordQuery a) => CrosswordDictionary -> a -> [String]

crossword (BidirectionalDictionary dict) word = map fromBidirect $ flip (doQuery dict) "" $ toBidirect $ convertQuery word

crossword (ForwardDictionary dict) word = flip (doQuery dict) "" $ convertQuery word

-- | Treat the input as a raw string, and query for subsequences of that string in the dictionary. Does not have metacharacters, and wraps every letter in an individual 'Opt'.
crosswordSubseq :: CrosswordDictionary -> String -> [String]
crosswordSubseq (BidirectionalDictionary dict) word = map fromBidirect $ doQuery dict (Literal "|":(map (Opt . (\a->[a]) . toLower) $ reverse word)) ""
crosswordSubseq (ForwardDictionary dict) word = doQuery dict (reverse $ map (Opt . (\a->[a]) . toLower) $ reverse word) ""

-- | Break a list into a list of lists of n possibly-empty spans such that all (==lst) $ concat (splitNTimes n lst) is true.
splitNTimes n lst = unfoldrM zz (lst,n)
        where zz (_,0) = [Nothing]
              zz (lst,1) = [Just (lst,("",0))]
              zz (lst,n) = [Just (a,(b,n-1))|k<-[0..length lst], let (a,b)=splitAt k lst]
-- | Return a list of every possible insertion of n copies of c into lst.
insertNCopies c n lst = intercalate c <$> splitNTimes (n+1) lst

-- | Do a crossword lookup for the input with n unknowns interspersed.
crosswordSuperseqN :: CrosswordDictionary -> Int -> String -> [String]
crosswordSuperseqN dict n i = insertNCopies "?" n i >>= crossword dict
