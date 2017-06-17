{-# LANGUAGE Trustworthy #-}

{-|
Module: AnswerChecker
Description: Check answers for puzzlehunt 2016 - Monsters et Manus - without seeing them.
 -}
module AnswerChecker ( 
        checkAnswer,
        checkAnswerFuzzy,
        Correctness(..),
) where
import Network.Wreq
import Text.Regex.TDFA
import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Data.List
import Data.Char
import System.IO.Unsafe
import Control.Exception

replace s d = map (\a->if s==a then d else a)

-- | Simple "are you correct?" type, with a 'Show' instance.
data Correctness = CORRECT | INCORRECT | NONEXISTANT deriving (Show)
toCorrectness True = CORRECT
toCorrectness False = INCORRECT
exToCorr :: SomeException -> Correctness
exToCorr e = NONEXISTANT

-- | Check an answer, assuming that it follows /exactly/ the puzzle-solution CSS class and uppercase pattern.
checkAnswer :: String -> String -> Correctness
checkAnswer puzzle answer = unsafePerformIO $
  handle (return . exToCorr) $ toCorrectness <$> (=~ ("<span class=\"puzzle-solution\">"++(toUpper <$> answer)++"</span>")) <$>
    ((view responseBody) <$> (get $ "http://solutions.monsters-et-manus.com/hunt/solution/" ++(replace ' ' '_' $ toLower <$> puzzle)++ ".html")) 

-- | Pattern match against the given answer, in uppercase, on it's own within any pair of HTML tags. Can leak answer information, use only if confident and 'checkAnswer' doesn't work.
checkAnswerFuzzy :: String -> String -> ByteString
checkAnswerFuzzy puzzle answer = (=~ ("<[^>]*>"++(toUpper <$> answer)++"</[^>]*>")) $ view responseBody $ unsafePerformIO (get $ "http://solutions.monsters-et-manus.com/hunt/solution/" ++(replace ' ' '_' $ toLower <$> puzzle)++ ".html")
