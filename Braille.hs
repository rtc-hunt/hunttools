module Braille where
import qualified Data.Map as Map
import qualified Data.Map.Lazy as Map
import Data.Bits
import Data.Bits.Bitwise (toListLE, fromListLE)
import Crossword

decade = "⠁⠃⠉⠙⠑⠋⠛⠓⠊⠚"
decade1 = decade
decade2 :: String
decade2 = (toEnum . ((.|. fromEnum '⠄')::(Int->Int)) . fromEnum) <$> decade
decade3 :: String
decade3 = (toEnum . ((.|. fromEnum '⠠')::(Int->Int)) . fromEnum) <$> decade2
decade4 :: String
decade4 = (toEnum . ((.|. fromEnum '⠠')::(Int->Int)) . fromEnum) <$> decade

bottomrow = '⠤'
middlerow = '⠒'
toprow = '⠉'

englishBrailleByDecade = ((:"") <$> "abcdefghijklmnopqrstuvxyz") ++ ["and", "for", "of", "the", "with"]
        ++ ["ch", "gh", "sh", "th", "wh", "ed", "er", "ou", "ow", "w"]

fromEnglishBrailleMap = Map.fromList $ zip (decade1 ++ decade2 ++ decade3 ++ decade4) englishBrailleByDecade
toEnglishBrailleMap = Map.fromList $ flip zip (decade1 ++ decade2 ++ decade3 ++ decade4) englishBrailleByDecade

fromEnglishBraille :: String -> String
fromEnglishBraille = (>>= (\a -> Map.findWithDefault [a] a fromEnglishBrailleMap))
toEnglishBraille :: String -> String
toEnglishBraille = ((\a -> Map.findWithDefault a [a] toEnglishBrailleMap) <$>)

bits c = fromEnum c - fromEnum '⠀'
unBits :: Int->Char
unBits c = toEnum $ c + fromEnum '⠀'

maskBitsScanner :: [Bool] -> [[Bool]]
maskBitsScanner (False:bitList) = do
        rest <- maskBitsScanner bitList
        return $ False:rest
maskBitsScanner (True:bitList) = do
        rest <- maskBitsScanner bitList
        bit <- [True, False]
        return $ bit:rest
maskBitsScanner [] = [[]]

maskCharPossib k c = unBits <$> (.|. (bits k .&. complement (bits c))) <$> fromListLE <$> (maskBitsScanner $ toListLE $ bits c)

makeBrailleMaskRegex known unknownMask = Charset <$> fromEnglishBraille <$> zipWith maskCharPossib known unknownMask

--makeBrailleMaskRegex known unknown = (fromEnglishBraille <$>) <$> maskCharPossib 

--plain = fromList 
