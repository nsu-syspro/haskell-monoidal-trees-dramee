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

data Direction = GoLeft | GoRight
  
isTerminal :: Tree m a -> Bool
isTerminal Empty = True
isTerminal (Leaf _) = True
isTerminal (Node2 _ l r) = case (l, r) of
  (Leaf _, Leaf _) -> True
  _ -> False
isTerminal (Node3 _ l m r) = case (l, m, r) of
  (Leaf _, Leaf _, Leaf _) -> True
  _ -> False


insertDir :: Measured m a => Direction -> a -> Tree m a -> InsertResult m a
insertDir _ a' Empty = Inserted (Leaf a')
insertDir dir a' (Leaf x) =
  case dir of
    GoLeft  -> Inserted (node2 (leaf a') (leaf x))
    GoRight -> Inserted (node2 (leaf x) (leaf a'))
insertDir dir a' node@(Node2 _ l r) =
  if isTerminal node then
    case dir of
    GoLeft  -> Inserted (node3 (leaf a') l r)
    GoRight -> Inserted (node3 l r (leaf a'))
  else
    case dir of
      GoLeft -> case insertDir dir a' l of 
        Inserted newL -> Inserted (node2 newL r)
        Split newL newM -> Inserted (node3 newL newM r)
      GoRight -> case insertDir dir a' r of
        Inserted newR -> Inserted (node2 l newR)
        Split newM newR -> Inserted (node3 l newM newR)
insertDir dir a' node@(Node3 _ l m r) =
  if isTerminal node then
    case dir of
    GoLeft  -> Split (node2 (leaf a') l) (node2 m r)
    GoRight -> Split (node2 l m) (node2 r (leaf a'))
  else
    case dir of
      GoLeft -> case insertDir dir a' l of 
        Inserted newL -> Inserted (node3 newL m r)
        Split newL newM -> Split (node2 newL newM) (node2 m r)
      GoRight -> case insertDir dir a' r of
        Inserted newR -> Inserted (node3 l m newR)
        Split newM newR -> Split (node2 l m) (node2 newM newR)

instance MonoidalTree Tree where
  toTree = foldl' (|>) Empty
  (<|) a tree = case insertDir GoLeft a tree of
    Inserted newTree -> newTree
    Split left right -> node2 left right 
  
  (|>) a tree = case insertDir GoRight tree a of
    Inserted newTree -> newTree
    Split left right -> node2 left right

