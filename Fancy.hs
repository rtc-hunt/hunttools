{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, MultiParamTypeClasses #-}

module Fancy where

import Data.Aeson
import Anagram
import GHC.Generics
import Data.List
import Control.Monad

data FancyDispatch
  = Choice [String] Bool
  | StringList [String] Bool
  | SomethingElse
  deriving (Generic, Show)

instance ToJSON FancyDispatch
instance FromJSON FancyDispatch

class HasFancy a where
  fancyToFrontend :: a -> Data.Aeson.Value

instance HasFancy (FancyCarrier a) where
  fancyToFrontend (FancyCarrier fd _) = toJSON fd

instance HasFancy [String] where
  fancyToFrontend lst = let (first30, remainder) = splitAt 30 lst
                         in toJSON $ StringList first30 $ not $ null remainder

data FancyCarrier a = FancyCarrier FancyDispatch (a->FancyCarrier a)
instance Show (FancyCarrier a) where
  show (FancyCarrier fd _) = show fd

{-
instance Monad (FancyCarrier a) where
  (FancyCarrier a anext) >>= (FancyCarrier b bnext) = FancyCarrier (a >>= b) (\q -> (anext q >>= bnext q))
  return a = FancyCarrier (return a) (const $ return a)

instance MonadPlus (FancyCarrier a) where
  mzero = FancyCarrier (mzero) (const $ mzero)
  mplus =
-}

anagramChooser :: AnagramDictionary -> String -> FancyCarrier ([String], Int)
anagramChooser dict str = anagramChooserSkip dict str 0

anagramChooserSkip :: AnagramDictionary -> String -> Int -> FancyCarrier ([String], Int)
anagramChooserSkip dict str skip =
  let dropped = drop (skip * 30) $ anagramAny dict str
      (first30, rem) = splitAt 30 dropped
      contFun (a, skip') = anagramChooserSkip dict (str \\ join a) skip'
   in FancyCarrier (Choice first30 $ not $ null rem) contFun

class CanApplyInput a b where
  applyInput :: a -> b -> a

instance CanApplyInput (FancyCarrier a) a where
  applyInput (FancyCarrier _ a) b = a b

instance CanApplyInput [String] Int where
  applyInput aStr b = drop (30*b) aStr
