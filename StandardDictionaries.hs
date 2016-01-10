module StandardDictionaries (sysDict) where
import Dictionary
import System.IO.Unsafe
import Data.Char
import qualified Crossword
import Data.DAWG.Packed
import Anagram


sysDictGen = Crossword.buildDict $ words $ map toLower $ unsafePerformIO $ readFile "/usr/share/dict/words"
sysDict = Dictionary 
	(buildAnagramDictionary $ map toLower $ unsafePerformIO $ readFile "/usr/share/dict/words")
	(unsafePerformIO $ fromFile "/home/jonored/hunttools/sysDict.dawg")
