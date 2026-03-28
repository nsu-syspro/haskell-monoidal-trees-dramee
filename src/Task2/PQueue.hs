{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2.PQueue where

import Common.PriorityQueue

import Task1 (Measured(..), MinMax(..), getMin, getMinMax, getMax)
import Task2.Tree
import Data.Foldable (Foldable(foldl'))
import Data.Maybe (fromJust)

-- * Priority queue definition

-- | Priority queue based on binary tree
newtype PQueue k v = PQueue { getTree :: Tree (MinMax k) (Entry k v) }
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry { getEntry :: (k, v)}
  deriving (Show, Eq)

instance Ord k => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = measure k

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty
  toPriorityQueue = foldl' (flip $ uncurry insert) empty
  entries pq = case getTree pq of
    Empty -> []
    Leaf e -> [getEntry e]
    Branch _ l r -> entries (PQueue l) ++ entries (PQueue r)
  insert k v pq = PQueue (insert' (Entry (k, v)) (getTree pq)) where
    getKey = fst . getEntry
    insert' :: Ord k => Entry k v -> Tree (MinMax k) (Entry k v) -> Tree (MinMax k) (Entry k v)
    insert' e Empty = leaf e
    insert' e (Leaf e')
      | getKey e <= getKey e'   = branch (leaf e) (leaf e')
      | otherwise    = branch (leaf e') (leaf e) 
    insert' e (Branch m l r)
      | getKey e < fromJust (getMin (fst (getMinMax m))) = branch (insert' e l) r
      | getKey e > fromJust (getMax (snd (getMinMax m))) = branch l (insert' e r)
      | otherwise = branch (insert' e l) r
      
         

  extractMin pq = case getTree pq of
    Empty -> Nothing
    Leaf e -> Just (snd (getEntry e), PQueue Empty)
    Branch _ l r -> case extractMin (PQueue l) of
      Just (v, newL) -> Just (v, PQueue $ branch (getTree newL) r)
      Nothing -> case extractMin (PQueue r) of
        Just (v, newR) -> Just (v, PQueue $ branch l (getTree newR))
        Nothing -> Nothing
  extractMax pq = case getTree pq of
    Empty -> Nothing
    Leaf e -> Just (snd (getEntry e), PQueue Empty)
    Branch _ l r -> case extractMax (PQueue r) of
      Just (v, newR) -> Just (v, PQueue $ branch l (getTree newR))
      Nothing -> case extractMax (PQueue l) of
        Just (v, newL) -> Just (v, PQueue $ branch (getTree newL) r)
        Nothing -> Nothing