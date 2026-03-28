{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Seq where

import Common.Sequence

import Task1 (Measured(..), Size(..))
import Task3.Tree
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

data RemoveResult m a
  = Removed (Tree m a)
  | Underflow (Tree m a)
      
instance Sequence Seq where
  empty = Seq Empty
  toSequence = foldl' (|+) empty
  (+|) = insertAt 0
  (|+) s a = insertAt (length s) a s
  insertAt n a s = Seq $ getFromInsertResult (insertAtTree n (Elem a) (getTree s))
    where
      getFromInsertResult :: InsertResult (Size a) (Elem a) -> Tree (Size a) (Elem a)
      getFromInsertResult (Inserted t) = t
      getFromInsertResult (Split l r) = node2 l r
      insertAtTree :: Int -> Elem a -> Tree (Size a) (Elem a) -> InsertResult (Size a) (Elem a)
      insertAtTree _ x Empty = Inserted (leaf x)
      insertAtTree m x (Leaf y)
        | m <= 0   = Inserted (node2 (leaf x) (leaf y))
        | otherwise = Inserted (node2 (leaf y) (leaf x))
      insertAtTree k x node@(Node2 _ l r)
        | isTerminal node = 
          if k <= 0 then
            Inserted (node3 (leaf x) l r)
          else if k == 1 then
            Inserted (node3 l (leaf x) r)
          else
            Inserted (node3 l r (leaf x))
        | k <= length (Seq l) = case insertAtTree k x l of
              Inserted newL -> Inserted (node2 newL r)
              Split newL newM -> Inserted (node3 newL newM r)
        | otherwise = case insertAtTree (k - length (Seq l)) x r of
              Inserted newR -> Inserted (node2 l newR)
              Split newM newR -> Inserted (node3 l newM newR)
      insertAtTree k x node@(Node3 _ l m r)
        | isTerminal node = 
          if k <= 0 then
            Split (node2 (leaf x) l) (node2 m r)
          else if k == 1 then
            Split (node2 l (leaf x)) (node2 m r)
          else if k == 2 then
            Split (node2 l m) (node2 (leaf x) r)
          else
            Split (node2 l m) (node2 r (leaf x))
        | k < length (Seq l) = case insertAtTree k x l of
              Inserted newL -> Inserted (node3 newL m r)
              Split newL newM -> Split (node2 newL newM) (node2 m r)
        | k > length (Seq l) + length (Seq m) = case insertAtTree (k - length (Seq l) - length (Seq m)) x r of
              Inserted newR -> Inserted (node3 l m newR)
              Split newM newR -> Split (node2 l m) (node2 newM newR)
        | otherwise = case insertAtTree (k - length (Seq l)) x m of 
              Inserted newM -> Inserted (node3 l newM r)
              Split newML newMR -> Split (node2 l newML) (node2 newMR r)
                  
  removeAt n s = Seq $ getFromRemovedResult (removeAtTree n (getTree s)) where 
    
    getFromRemovedResult :: RemoveResult (Size a) (Elem a) -> Tree (Size a) (Elem a)
    getFromRemovedResult (Removed t) = t
    getFromRemovedResult (Underflow t) = t

    removeAtTree :: Int -> Tree (Size a) (Elem a) -> RemoveResult (Size a) (Elem a)
    removeAtTree _ Empty = Removed Empty  
    removeAtTree k (Leaf x) = if k == 0 then Removed Empty else Removed (Leaf x)
    removeAtTree k node@(Node2 _ l r)
      | isTerminal node =
        if k == 0 then
          Underflow r 
        else if k == 1 then
          Underflow l
        else 
          Removed node
      | k < length (Seq l) = case removeAtTree k l of
          Removed newL -> Removed (node2 newL r)
          Underflow newL -> case r of
            Node2 _ m r' -> Underflow (node3 newL m r')
            Node3 _ l' m r' -> Removed (node2 (node2 newL l') (node2 m r'))
            _ -> error "WTF: right child of a non-terminal Node2 should be a Node2 or Node3"
      | otherwise = case removeAtTree (k - length (Seq l)) r of
          Removed newR -> Removed (node2 l newR)
          Underflow newR -> case l of
            Node2 _ l' m -> Underflow (node3 l' m newR)
            Node3 _ l' m1 r' -> Removed (node2 (node2 l' m1) (node2 r' newR))
            _ -> error "WTF: left child of a non-terminal Node2 should be a Node2 or Node3"
    removeAtTree k node@(Node3 _ l m r)
      | isTerminal node =
        if k == 0 then
          Removed (node2 m r)
        else if k == 1 then
          Removed (node2 l r)
        else if k == 2 then
          Removed (node2 l m)       
        else  
          Removed node
      | k < length (Seq l) = case removeAtTree k l of
          Removed newL -> Removed (node3 newL m r)
          Underflow newL -> case m of
            Node2 _ l' r' -> Removed (node2 (node3 newL l' r') r)
            Node3 _ l' m' r' -> Removed (node3 (node2 newL l') (node2 m' r') r)
            _ -> error "WTF: middle child of a non-terminal Node3 should be a Node2 or Node3"
      | k >= length (Seq l) + length (Seq m) = case removeAtTree (k - length (Seq l) - length (Seq m)) r of
          Removed newR -> Removed (node3 l m newR)
          Underflow newR -> case m of
            Node2 _ l' r' -> Removed (node2 l (node3 l' r' newR))
            Node3 _ l' m' r' -> Removed (node3 l (node2 l' m') (node2 r' newR))
            _ -> error "WTF: middle child of a non-terminal Node3 should be a Node2 or Node3"
        | otherwise = case removeAtTree (k - length (Seq l)) m of
          Removed newM -> Removed (node3 l newM r)
          Underflow newM -> case l of
            Node2 _ l' m' -> Removed (node2 (node3 l' m' newM) r)
            Node3 _ l' m' r' -> Removed (node3 (node2 l' m') (node2 r' newM) r)
            _ -> error "WTF: left child of a non-terminal Node3 should be a Node2 or Node3"

  elemAt n s = elemAtTree n (getTree s) where
    elemAtTree :: Int -> Tree (Size a) (Elem a) -> Maybe a
    elemAtTree _ Empty = Nothing
    elemAtTree k (Leaf (Elem x)) = if k == 0 then Just x else Nothing 
    elemAtTree k (Node2 _ l r)
      | k < length (Seq l) = elemAtTree k l
      | otherwise = elemAtTree (k - length (Seq l)) r
    elemAtTree k (Node3 _ l m r)
      | k < length (Seq l) = elemAtTree k l
      | k >= length (Seq l) + length (Seq m) = elemAtTree (k - length (Seq l) - length (Seq m)) r
      | otherwise = elemAtTree (k - length (Seq l)) m
