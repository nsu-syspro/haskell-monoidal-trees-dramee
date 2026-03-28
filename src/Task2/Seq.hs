{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2.Seq where

import Common.Sequence

import Task1 (Measured(..), Size(..))
import Task2.Tree
import Data.Foldable (Foldable(foldl'))

-- * Sequence definition

-- | Random-access sequence based on binary tree
newtype Seq a = Seq { getTree :: Tree (Size a) (Elem a) }
  deriving (Show, Eq)

-- | Sequence element wrapper
newtype Elem a = Elem { getElem :: a }
  deriving (Show, Eq)

-- | Measures given element as 'Size 1'
instance Measured (Size a) (Elem a) where
  measure = Size . const 1

instance Measured (Size a) (Seq a) where
  measure = measure . getTree

instance Foldable Seq where
  foldMap f s = foldMap (f . getElem) (getTree s)

  -- An O(1) implementation of length is possible
  -- due to size of the tree being cached at each node
  length :: forall a. Seq a -> Int
  length = getSize . (measure @(Size a))
-- * Sequence instance



instance Sequence Seq where
  empty = Seq Empty
  toSequence = foldl' (|+) empty
  (+|) = insertAt 0
  (|+) s a = insertAt (length s) a s
  insertAt n a s = Seq $ insertAt' n (Elem a) (getTree s) where
    insertAt' :: Int -> Elem a -> Tree (Size a) (Elem a) -> Tree (Size a) (Elem a)
    insertAt' _ x Empty = leaf x
    insertAt' m x (Leaf y)
      | m <= 0   = branch (leaf x) (leaf y)
      | otherwise = branch (leaf y) (leaf x)
    insertAt' k x (Branch _ l r)
      | k < length (Seq l) = branch (insertAt' k x l) r
      | otherwise   = branch l (insertAt' (k - length (Seq l)) x r)
  removeAt :: Int -> Seq a -> Seq a
  removeAt n a = Seq $ removeAt' n (getTree a) where
    removeAt' :: Int -> Tree (Size a) (Elem a) -> Tree (Size a) (Elem a)
    removeAt' _ Empty = Empty
    removeAt' m (Leaf x)
      | m == 0    = Empty
      | otherwise = Leaf x
    removeAt' k (Branch _ l r)
      | k < length (Seq l)  = branch (removeAt' k l) r
      | otherwise   = branch l (removeAt' (k - length (Seq l)) r)
  elemAt n s = case getTree s of
    Empty -> Nothing
    Leaf (Elem x)
      | n == 0    -> Just x
      | otherwise -> Nothing
    Branch _ l r
      | n < length (Seq l) -> elemAt n (Seq l)
      | otherwise    -> elemAt (n - length (Seq l)) (Seq r)