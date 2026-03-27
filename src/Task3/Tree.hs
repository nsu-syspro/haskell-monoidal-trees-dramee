{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Tree where

import Common.MonoidalTree

import Task1 (Measured(..))
import Data.Foldable (Foldable(foldl'))

-- * 2-3 tree definition

-- | 2-3 tree with values 'a' in leaves
-- Intermediate nodes contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Leaf a
  | Node2 m (Tree m a) (Tree m a)
  | Node3 m (Tree m a) (Tree m a) (Tree m a)
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance Measured m a => Measured m (Tree m a) where
  measure Empty = mempty
  measure (Leaf x) = measure x
  measure (Node2 m _ _) = m
  measure (Node3 m _ _ _) = m

instance Foldable (Tree m) where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node2 _ l r) = foldMap f l <> foldMap f r
  foldMap f (Node3 _ l m r) = foldMap f l <> foldMap f m <> foldMap f r

-- * Smart constructors

leaf :: a -> Tree m a
leaf = Leaf

node2 :: Measured m a => Tree m a -> Tree m a -> Tree m a
node2 l r = Node2 (measure l <> measure r) l r

node3 :: Measured m a => Tree m a -> Tree m a -> Tree m a -> Tree m a
node3 l m r = Node3 (measure l <> measure m <> measure r) l m r

-- * Monoidal tree instance

data InsertResult m a
  = Inserted (Tree m a)
  | Split (Tree m a) (Tree m a)

instance MonoidalTree Tree where
  toTree = foldl' (|>) Empty
  (<|) a tree = case insert a tree of
    Inserted newTree -> newTree
    Split left right -> node2 left right 
    where
      insert :: Measured m a => a -> Tree m a -> InsertResult m a
      insert a' Empty = Inserted (Leaf a')
      insert a' (Leaf x) = Inserted (node2 (leaf a') (leaf x))
      insert a' (Node2 _ l r) = Inserted (node3 (leaf a') l r)
      insert a' (Node3 _ l m r) = case insert a' l of
        Inserted newL -> Inserted (node3 newL m r)
        Split newL newM -> Inserted (node2 newL (node3 newM m r))
  (|>) a tree = case insert a tree of
    Inserted newTree -> newTree
    Split left right -> node2 left right 
    where
      insert :: Measured m a => Tree m a -> a -> InsertResult m a
      insert Empty a' = Inserted (Leaf a')
      insert (Leaf x) a' = Inserted (node2 (leaf x) (leaf a'))
      insert (Node2 _ l r) a' = Inserted (node3  l r (leaf a'))
      insert (Node3 _ l m r) a' = case insert r a' of
        Inserted newR -> Inserted (node3 l m newR)
        Split newM newR -> Inserted (node2  (node3 l m newM) newR)


