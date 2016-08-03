{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Dictionary (Dictionary (Dictionary), anaDict, crossDict, DictionaryContext, fromDictionary) where
import Anagram
import Crossword
import Data.DAWG.Packed64

class DictionaryContext a where
  fromDictionary :: Dictionary -> a

instance (DictionaryContext CrosswordDictionary) where
  fromDictionary dict = crossDict dict

instance (DictionaryContext AnagramDictionary) where
  fromDictionary dict = anaDict dict

instance (DictionaryContext Dictionary) where
  fromDictionary dict = dict

data Dictionary = Dictionary {anaDict :: AnagramDictionary, crossDict :: CrosswordDictionary}

--instance (DictionaryContext a) => (Show a) where
--	show = (\a -> "<Dictionary>")::(DictionaryContext a)=>a->String

--instance Show Dictionary where
--	show d = "<Dictionary>"
