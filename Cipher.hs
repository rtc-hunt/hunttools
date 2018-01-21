{-# LANGUAGE Safe #-}
{-|
 Module: Cipher
 Description: Some very basic cipher tools.

 Currently all that's here are rot n-related ciphers, but more to be added.
 -}
module Cipher (rot, rotAll, a1b2, a1b2', a0b1, vignere, vignere', frequencies, substitute, invertSubstitute, unsubstitute) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Ord

rotChar :: Int -> Char -> Char
rotChar n c = toEnum $ (fromEnum 'a') + rem (n + ((fromEnum $ toLower c) - (fromEnum 'a'))) 26

-- | rotN cipher.
rot n = map $ rotChar n
-- | Brute-force a rotN cipher, by showing a list of each rotation. If there's a correct answer, it should be easy enough to pick out by eye.
rotAll a = [rot n a | n<-[1..25]]

a1b2posBeforeA = fromEnum 'a' - 1

-- | Map a string to numbers with a=1, b=2 substitution.
a1b2 :: String -> [Int]
a1b2 = fmap $ (subtract a1b2posBeforeA) . fromEnum
-- | map /from/ a=1 b=2 substitution. Does mod-26 internally.
a1b2' :: [Int] -> String
a1b2' = fmap $ toEnum . (+ a1b2posBeforeA) . (+ 1) . (`mod` 26) . (+ 259)

-- | 0-indexed a=0 b=1
a0b1 :: String -> [Int]
a0b1 = fmap $ (subtract $ fromEnum 'a') . fromEnum

a0b1' :: [Int] -> String
a0b1' = fmap $ toEnum . (+ (fromEnum 'a')) . (`mod` 26) . (+ 260)

-- | Vignère cipher, forward version.
vignere  :: String -> String -> String
vignere  key = a1b2' . (zipWith (+) $ cycle $ a0b1 key) . a1b2

-- | Vignère cipher, backward version.
vignere' :: String -> String -> String
vignere'  key = a1b2' . (zipWith subtract $ cycle $ a0b1 key) . a1b2

frequencies :: String -> [Float]
frequencies str = ((/ len). fromIntegral . length) <$> (group $ sort $ str)
        where len = fromIntegral $ length str :: Float

substitute :: [a] -> String -> [a]
substitute key str = (key!!) <$> a0b1 str

invertSubstitute :: String -> String
invertSubstitute = fmap fst . sortBy (comparing snd) . zip ['a'..'z']

unsubstitute :: (Eq a) => [a] -> [a] -> String
unsubstitute key str = a0b1' $ ((fromJust . flip elemIndex key) <$> str)
