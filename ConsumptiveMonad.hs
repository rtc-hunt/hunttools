{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module ConsumptiveMonad where 
import ListT
import Control.Monad.State
import Data.List
import Anagram

type QConsumer q a = ListT (State q) a
 
runQC :: q -> QConsumer q a -> [a]
runQC q = (flip evalState q) . toList

--isComplete = do
--        q <- get
--        return $ q == toQuery ""

complete a = do
        q <- get
        guard $ q == toQuery ""
        return a

class IsQuery a where
  (<\\>) :: a->String->a
  toQuery :: String->a

instance IsQuery String where
  (<\\>) = (\\)
  toQuery = id

instance IsQuery Histogram where
  h <\\> s = h `subHistogram` getHistogram s
  toQuery = getHistogram

-- Enables transparent get & put.
instance (MonadState s m) => MonadState s (ListT m) where
        get = lift get
        put = lift . put
        state = lift . state

consumer :: IsQuery q => (q->[String])->QConsumer q String
consumer f = do
        query <- get
        res <- fromFoldable $ f query
        put $ query <\\> res
        return res

instance (Show a, IsQuery s) => Show (QConsumer s a) where
        show = show . runQC (toQuery "DEFAULT QUERY")
