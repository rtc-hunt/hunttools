{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
 Module: ConsumptiveMonad
 Description: a monad for query-consuming operations, like multi-word anagrams or word deinterlacing.

 FIXME: figure out how to teach how to use this.

 >>> nub $ sort $ runQC "jheaarnrlyupcoptitcearrd" $ 
         do a <- consumer $ crosswordSubseq onelook
            b <- consumer $ crossword onelook
            return (a,b)
 [("harrypotter","jeanlucpicard"),("jeanlucpicard","harrypotter")]

 (nub & sort are there to remove duplicates derived from multiple ways to remove "jeanlucpicard" from "jheaarnrlyupcoptitcearrd".)

 >>> runQC "bceekkmnotuy" $ (replicateM 2 $ consumer $ anagramAny sysDict) >>= complete
 [["betoken","mucky"],["bucket","monkey"],["monkey","bucket"],["mucky","betoken"]]
 
 -}
module ConsumptiveMonad where 
import ListT
import Control.Monad.State
import Data.List
import Anagram
import Control.Applicative

type QConsumer q a = ListT (State q) a
 

-- | Run a QConsumer computation with a given query.
runQC :: q -> QConsumer q a -> [a]
runQC q = (flip evalState q) . toList

-- | Prune any solutions that have not consumed the whole query, but keep what the solution actually /is/, unlike raw guard.
complete a = do
        q <- get
        guard $ q == toQuery ""
        return a

class IsQuery a where
  (<\\>) :: a->String->QConsumer q a
  toQuery :: String->a

instance IsQuery String where
  (<\\>) = deleteSubseqOr
  toQuery = id

-- | Either delete all subsequences that matches the second argument (nondeterministically), or remove the first occurance of each element of b if b is not a subsequence of a.
deleteSubseqOr a b =
        let dssR = deleteSubseq a b
          in do
             qq <- lift . ListT.null $ dssR
             if qq
                then return $ a \\ b
                else dssR

-- | Delete b from a, where b is a subsequence of a, in every possible manner.
deleteSubseq :: (Eq a) => [a]->[a]->QConsumer q [a]
deleteSubseq q [] = return q;
deleteSubseq [] _ = empty;
deleteSubseq a@(ah:ar) b@(bh:br)
  = if ah == bh
      then
        deleteSubseq ar br <|> ((ah :) `fmap` deleteSubseq ar b) -- We could match here, so do both.
      else
        (ah :) `fmap` deleteSubseq ar b -- We didn't match, so we keep this letter and move on.

instance IsQuery Histogram where
  h <\\> s = return $ h `subHistogram` getHistogram s
  toQuery = getHistogram

-- Enables transparent get & put.
-- instance (MonadState s m) => MonadState s (ListT m) where
--        get = lift get
--        put = lift . put
--        state = lift . state

-- | Wrap a q -> [String] function to operate in the QConsumer monad.
consumer :: IsQuery q => (q->[String])->QConsumer q String
consumer f = do
        query <- get
        res <- fromFoldable $ f query
        specRemainder <- query <\\> res
        put specRemainder
        return res

-- instance (Show a, IsQuery s) => Show (QConsumer s a) where
--        show = show . runQC (toQuery "DEFAULT QUERY")
