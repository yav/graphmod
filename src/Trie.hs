module Trie where

import Control.Monad(msum)

data Trie a b = Sub [(a, Trie a b)] (Maybe b)
                deriving Show

empty :: Trie a b
empty = Sub [] Nothing

lookup :: Eq a => [a] -> Trie a b -> Maybe b
lookup [] (Sub _ b)       = b
lookup (k:ks) (Sub as _)  = Trie.lookup ks =<< Prelude.lookup k as

lookupPrefix :: Eq a => [a] -> Trie a b -> Maybe ([a],b)
lookupPrefix ks     (Sub _ (Just b)) = Just ([],b)
lookupPrefix (k:ks) (Sub ts _)       = do t <- Prelude.lookup k ts
                                          (as,v) <- lookupPrefix ks t
                                          return ((k:as),v)
lookupPrefix []     (Sub _ _)        = Nothing


insert :: Ord a => [a] -> (Maybe b -> b) -> Trie a b -> Trie a b
insert [] f (Sub as b)     = Sub as (Just (f b))
insert (k:ks) f (Sub as b) = Sub (upd as) b

  where upd ((k1,t):ss)
          | k1 < k    = (k1,t) : upd ss
          | k1 == k   = (k1, insert ks f t) : ss
        upd ss = (k, insert ks f empty) : ss

find :: (b -> Maybe c) -> Trie a b -> Maybe c
find p (Sub ts b) = msum ((p =<< b) : map (find p . snd) ts)




