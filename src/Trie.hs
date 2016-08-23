module Trie where

import qualified Data.Map as Map
import Data.Maybe(fromMaybe)

data Trie a b = Sub (Map.Map a (Trie a b)) (Maybe b)
                deriving (Eq, Ord, Show)

empty :: Trie a b
empty = Sub Map.empty Nothing

lookup :: Ord a => [a] -> Trie a b -> Maybe b
lookup [] (Sub _ b)       = b
lookup (k:ks) (Sub as _)  = Trie.lookup ks =<< Map.lookup k as

insert :: (Ord a) => [a] -> (Maybe b -> b) -> Trie a b -> Trie a b
insert [] f (Sub as b)     = Sub as (Just (f b))
insert (k:ks) f (Sub as b) = Sub (Map.alter upd k as) b
  where upd j = Just $ insert ks f $ fromMaybe empty j

instance Functor (Trie a) where
  fmap f (Sub m mb) = Sub (fmap (fmap f) m) (fmap f mb)
