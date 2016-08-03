{-# LANGUAGE Trustworthy #-}
module StandardDictionaries (sysDict, ukacd, enwikt, sowpods, wiki, onelook, onelookR) where
import Dictionary
import System.IO.Unsafe
import Data.Char
import qualified Crossword
import Data.DAWG.Packed64
import Anagram


sysDictGen = Crossword.buildDict $ words $ map toLower $ unsafePerformIO $ readFile "/usr/share/dict/words"
sysDictDef = Dictionary 
	(AnagramDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/dictWordsAnagram.dawg")
	(Crossword.BidirectionalDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/dictWordsCross.dawg")

sysDict :: (DictionaryContext a) => a
sysDict = fromDictionary sysDictDef

ukacdDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/ukacd-anagram.dawg")
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/ukacd-cross.dawg")

ukacd :: (DictionaryContext a) => a
ukacd = fromDictionary ukacdDictDef

enwiktDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/enwikt-anagram.dawg")
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/enwikt-cross.dawg")

enwikt :: (DictionaryContext a) => a
enwikt = fromDictionary enwiktDictDef

sowpodsDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/sowpods-anagram.dawg")
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/sowpods-cross.dawg")

sowpods :: (DictionaryContext a) => a
sowpods = fromDictionary sowpodsDictDef

wikiDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/wiki-anagram.dawg")
  (Crossword.ForwardDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/wiki-cross.dawg")

wiki :: (DictionaryContext a) => a
wiki = fromDictionary wikiDictDef

onelookDictDefR = Dictionary
  (AnagramDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/onelook-justletters.anagram.dawg")
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/onelook-justletters.cross.dawg")
onelookR :: (DictionaryContext a) => a
onelookR = fromDictionary onelookDictDefR

onelookDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ mapFile "/home/jonored/huntrepl/dicts/onelook-justletters.anagram.mm.dawg")
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ mapFile "/home/jonored/huntrepl/dicts/onelook-justletters.cross.mm.dawg")
onelook :: (DictionaryContext a) => a
onelook = fromDictionary onelookDictDef
