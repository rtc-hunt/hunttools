{-# Language OverloadedLists, TupleSections #-}
module Scrabble where
import qualified Data.Map as Map
import Data.Char

englishPoints :: Map.Map Char Int
englishPoints = Map.fromList $ (\(a,i)->[(a,i),(toUpper a,i)]) =<<
        (   (,1 ) <$> "eaionrtlsu")
        ++ ((,2 ) <$> "dg")
        ++ ((,3 ) <$> "cmbp")
        ++ ((,4 ) <$> "hfwyv")
        ++ ((,5 ) <$> "k")
        ++ ((,8 ) <$> "jx")
        ++ ((,10) <$> "qz")

points = englishPoints

