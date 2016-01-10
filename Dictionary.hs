module Dictionary (Dictionary (Dictionary), anaDict, crossDict) where
import Anagram
import Crossword

data Dictionary = Dictionary {anaDict :: AnagramDictionary, crossDict :: CrosswordDictionary}
instance Show Dictionary where
	show d = "<Dictionary>"
