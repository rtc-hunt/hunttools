{-# LANGUAGE Trustworthy #-}
module StandardDictionaries where
--module StandardDictionaries (sysDict, ukacd, onelook, sowpods) where
import Dictionary
import System.IO.Unsafe
import Data.Char
import qualified Crossword
import Data.DAWG.Packed64
import Anagram
import Paths_hunttools_dicts
import qualified Paths_hunttools as H
-- import qualified Language.Lojban.Jbovlaste as JBO

sysDictGen = Crossword.buildDict $ words $ map toLower $ unsafePerformIO $ readFile "/usr/share/dict/words"
sysDictDef = Dictionary 
	(AnagramDictionary $ unsafePerformIO $ getDataFileName "dictWordsAnagram.dawg" >>= fromFile)
	(Crossword.BidirectionalDictionary $ unsafePerformIO $ getDataFileName "dictWordsCross.dawg" >>= fromFile)

sysDict :: (DictionaryContext a) => a
sysDict = fromDictionary sysDictDef

ukacdDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ getDataFileName "ukacd-anagram.dawg" >>= fromFile)
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ getDataFileName "ukacd-cross.dawg" >>= fromFile)

ukacd :: (DictionaryContext a) => a
ukacd = fromDictionary ukacdDictDef

{-enwiktDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/enwikt-anagram.dawg")
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/enwikt-cross.dawg")

enwikt :: (DictionaryContext a) => a
enwikt = fromDictionary enwiktDictDef


wikiDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/wiki-anagram.dawg")
  (Crossword.ForwardDictionary $ unsafePerformIO $ fromFile "/home/jonored/huntrepl/dicts/wiki-cross.dawg")

wiki :: (DictionaryContext a) => a
wiki = fromDictionary wikiDictDef

onelookDictDefR = Dictionary
  (AnagramDictionary $ unsafePerformIO $ fromFile $ getDataFileName "/home/jonored/huntrepl/dicts/onelook-justletters.anagram.dawg")
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ fromFile $ getDataFileName "/home/jonored/huntrepl/dicts/onelook-justletters.cross.dawg")
onelookR :: (DictionaryContext a) => a
onelookR = fromDictionary onelookDictDefR
-}

onelookDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ getDataFileName "onelook-justletters.anagram.mm.dawg" >>= mapFile)
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ getDataFileName "onelook-justletters.cross.mm.dawg" >>= mapFile)
onelook :: (DictionaryContext a) => a
onelook = fromDictionary onelookDictDef

{-sowpodsDictDef = Dictionary
  (AnagramDictionary $ unsafePerformIO $ getDataFileName "sowpods-anagram.dawg" >>= fromFile)
  (Crossword.BidirectionalDictionary $ unsafePerformIO $ getDataFileName "sowpods-cross.dawg" >>= fromFile)

sowpods :: (DictionaryContext a) => a
sowpods = fromDictionary sowpodsDictDef
-}
-- jbovlaste = unsafePerformIO $ H.getDataFileName "jbovlaste.db" >>= JBO.readDB
