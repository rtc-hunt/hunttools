{-# LANGUAGE Trustworthy #-}
module AnswerChecker where
import Network.Wreq
import Text.Regex.TDFA
import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Data.List
import Data.Char
import System.IO.Unsafe
import Control.Exception

replace s d = map (\a->if s==a then d else a)

data Correctness = CORRECT | INCORRECT | NONEXISTANT deriving (Show)
toCorrectness True = CORRECT
toCorrectness False = INCORRECT
exToCorr :: SomeException -> Correctness
exToCorr e = NONEXISTANT

checkAnswer :: String -> String -> Correctness
checkAnswer puzzle answer = unsafePerformIO $
  handle (return . exToCorr) $ toCorrectness <$> (=~ ("<span class=\"puzzle-solution\">"++(toUpper <$> answer)++"</span>")) <$>
    ((view responseBody) <$> (get $ "http://solutions.monsters-et-manus.com/hunt/solution/" ++(replace ' ' '_' $ toLower <$> puzzle)++ ".html")) 

checkAnswerFuzzy :: String -> String -> ByteString
checkAnswerFuzzy puzzle answer = (=~ ("<[^>]*>"++(toUpper <$> answer)++"</[^>]*>")) $ view responseBody $ unsafePerformIO (get $ "http://solutions.monsters-et-manus.com/hunt/solution/" ++(replace ' ' '_' $ toLower <$> puzzle)++ ".html")
