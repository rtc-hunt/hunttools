{-# Language OverloadedLists, TupleSections #-}

{-|
 Module: Scrabble
 Description: Scrabble point scores.
 -}
module Scrabble where
import qualified Data.Map as Map
import Data.Char

-- | A map of scrabble letter to score for english.
englishPoints :: Map.Map Char Int
englishPoints = Map.fromList $ (\(a,i)->[(a,i),(toUpper a,i)]) =<<
        (   (,1 ) <$> "eaionrtlsu")
        ++ ((,2 ) <$> "dg")
        ++ ((,3 ) <$> "cmbp")
        ++ ((,4 ) <$> "hfwyv")
        ++ ((,5 ) <$> "k")
        ++ ((,8 ) <$> "jx")
        ++ ((,10) <$> "qz")

-- | a synonym for 'englishPoints'
points = englishPoints

