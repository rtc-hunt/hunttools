module Extract where

import Safe
import Crossword
import Data.List
import Data.Maybe

diagonalize :: [String] -> Maybe String
diagonalize = sequence . zipWith (flip atMay) [0..]

diagonalizeUnsorted :: CrosswordDictionary -> [String] -> [String]
diagonalizeUnsorted dict words = permutations words >>= maybeToList . diagonalize >>= crossword dict
