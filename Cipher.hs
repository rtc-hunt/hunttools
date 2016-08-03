{-# LANGUAGE Safe #-}
module Cipher (rot, rotAll) where

import Data.Char

rotChar :: Int -> Char -> Char
rotChar n c = toEnum $ (fromEnum 'a') + rem (n + ((fromEnum $ toLower c) - (fromEnum 'a'))) 26

rot n = map $ rotChar n
rotAll a = [rot n a | n<-[1..25]]
