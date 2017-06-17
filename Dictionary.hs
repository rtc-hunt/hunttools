{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-|
 Module: Dictionary
 Description: Select the correct preprocessed form of a dictionary automatically based on the query type

 We use a simple return type polymorphism mechanism to select the appropriate data structure for the query, so we have things like
 @ukacd :: (DictionaryContext a) => a@ where @a@ can be either CrosswordDictionary or AnagramDictionary. The definition for ukacd is then something like

 > ukacd = fromDictionary $ Dictionary (AnagramDictionary $ fromFile "ukacd-anagram.dawg") (Crossword.BidirectionalDictionary $ fromFile "ukacd-cross.dawg")
 (not literally that, there are some unsafePerformIO (We know that these files are effectively large constants), "where is this file installed", etc. steps, but that is roughly the correct intuition.)
-}
module Dictionary (Dictionary (Dictionary), anaDict, crossDict, DictionaryContext, fromDictionary) where
import Anagram
import Crossword
import Data.DAWG.Packed64

-- | A context that we might want to have a specialized dictionary representation for.
class DictionaryContext a where
  fromDictionary :: Dictionary -> a

-- | Crossword dictionaries are one form
instance (DictionaryContext CrosswordDictionary) where
  fromDictionary dict = crossDict dict

-- | Anagram dictionaries are another
instance (DictionaryContext AnagramDictionary) where
  fromDictionary dict = anaDict dict

-- | A context for the raw Dictionary structure; this lets us still use the ukacd term when we /don't/ want to do dispatch, and instead access all of the dictionaries available.
instance (DictionaryContext Dictionary) where
  fromDictionary dict = dict

-- | Effectively, a Dictionary object consists of a copy of the dictionary structured for anagrams and the same dictionary structured for crossword queries. If we find that there are more data structures that are useful and we want all the dictionaries in, we can add them here (and add an appropriate 'DictionaryContext' instance).
data Dictionary = Dictionary {anaDict :: AnagramDictionary, crossDict :: CrosswordDictionary}

--instance (DictionaryContext a) => (Show a) where
--	show = (\a -> "<Dictionary>")::(DictionaryContext a)=>a->String

--instance Show Dictionary where
--	show d = "<Dictionary>"
