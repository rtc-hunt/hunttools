{-# LANGUAGE Safe #-}
{-|
 Module: Cipher
 Description: Some very basic cipher tools.

 Currently all that's here are rot n-related ciphers, but more to be added.
 -}
module Cipher (rot, rotAll) where

import Data.Char

rotChar :: Int -> Char -> Char
rotChar n c = toEnum $ (fromEnum 'a') + rem (n + ((fromEnum $ toLower c) - (fromEnum 'a'))) 26

-- | rotN cipher.
rot n = map $ rotChar n
-- | Brute-force a rotN cipher, by showing a list of each rotation. If there's a correct answer, it should be easy enough to pick out by eye.
rotAll a = [rot n a | n<-[1..25]]
