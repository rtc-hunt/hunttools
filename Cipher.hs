{-# LANGUAGE Safe #-}
{-|
 Module: Cipher
 Description: Some very basic cipher tools.

 Currently all that's here are rot n-related ciphers, but more to be added.
 -}
module Cipher (rotChar, rot, rotAll, vignere, vignere', a1b2, a1b2', a0b1, a0b1') where

import Data.Char

rotChar :: Int -> Char -> Char
rotChar n c = toEnum $ (fromEnum 'a') + rem (n + ((fromEnum $ toLower c) - (fromEnum 'a'))) 26

-- | rotN cipher.
rot n = map $ rotChar n
-- | Brute-force a rotN cipher, by showing a list of each rotation. If there's a correct answer, it should be easy enough to pick out by eye.
rotAll a = [rot n a | n<-[1..25]]

-- | Vignere cipher encrypt
vignere key plaintext = zipWith rotChar (cycle $ a0b1 key) plaintext
-- | Vignere cipher decrypt
vignere' key ciphertext = zipWith rotChar (cycle $ negate <$> a0b1 key) ciphertext

-- | a=1 b=2 ec.
a1b2 = fmap (+1) . a0b1
-- | Reverse a=1 b=2
a1b2' = a0b1' . fmap (subtract 1)

-- | a=0 b=1 etc.
a0b1 :: String -> [Int]
a0b1 = fmap $ subtract (fromEnum 'a') . fromEnum . toLower

-- | Reverse a=0 b=1 etc.
a0b1' :: [Int] -> String
a0b1' = fmap $ toEnum . (+) (fromEnum 'a')
