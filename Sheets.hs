{-# LANGUAGE Trustworthy #-}
module Sheets (doGetRange, runTotallyNotIO) where

import Network.Google.Sheets
import Network.Google
import Control.Lens
import System.IO.Unsafe

data TotallyNotIO a = TotallyNotIO (IO a)
runTotallyNotIO (TotallyNotIO a) = a
instance Monad TotallyNotIO where
        return a = TotallyNotIO $ return a
        (TotallyNotIO a)>>=b = TotallyNotIO $ (a>>=runTotallyNotIO . b)
instance Applicative TotallyNotIO
instance Functor TotallyNotIO where
        fmap a (TotallyNotIO b) = TotallyNotIO $ fmap a b

doGetRange id range = TotallyNotIO $ do
        e <- newEnv <&> (envScopes <>~ spreadsheetsReadOnlyScope)
        runResourceT . runGoogle e $
          send $ spreadsheetsValuesGet id range
