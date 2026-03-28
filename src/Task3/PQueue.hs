{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.PQueue where

import Common.PriorityQueue

import Task1 (Measured(..), MinMax(..), getMin, getMinMax, getMax)
import Task3.Tree
import Data.Foldable (Foldable(foldl'))
import Data.Maybe (fromJust)
import Task3.Seq (RemoveResult (..))


-- * Priority queue definition

-- | Priority queue based on binary tree
newtype PQueue k v = PQueue { getTree :: Tree (MinMax k) (Entry k v) }
  deriving (Show, Eq)


-- | Priority queue entry wrapper
newtype Entry k v = Entry { getEntry :: (k, v) }
  deriving (Show, Eq)

instance Ord k => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = measure k

-- * Priority queue instance

getFromRemoveResult :: Ord k => RemoveResult (MinMax k) (Entry k v) -> Tree (MinMax k) (Entry k v)
getFromRemoveResult (Removed t) = t
getFromRemoveResult (Underflow t) = t



getFromMaybeRemoveResult :: Ord k => Maybe (v, RemoveResult (MinMax k) (Entry k v)) -> Maybe (v, PQueue k v)
getFromMaybeRemoveResult Nothing = Nothing
getFromMaybeRemoveResult (Just (v, newTree)) = Just (v, PQueue $ getFromRemoveResult newTree)

getValueFromLeaf :: Tree (MinMax k) (Entry k v) -> Maybe v
getValueFromLeaf (Leaf e) = Just (snd (getEntry e))
getValueFromLeaf _ = Nothing

getEntryFromLeaf :: Tree (MinMax k) (Entry k v) -> Maybe (k, v)
getEntryFromLeaf (Leaf e) = Just (getEntry e)
getEntryFromLeaf _ = Nothing

instance PriorityQueue PQueue where
  empty = PQueue Empty
  toPriorityQueue = foldl' (flip $ uncurry insert) empty
  entries (PQueue Empty) = []
  entries (PQueue (Leaf e)) = [getEntry e]
  entries (PQueue (Node2 _ l r)) = entries (PQueue l) ++ entries (PQueue r)
  entries (PQueue (Node3 _ l m r)) = entries (PQueue l) ++ entries (PQueue m) ++ entries (PQueue r)
  insert k v pq = PQueue $ getFromInsertResult (insert' (Entry (k, v)) (getTree pq))
    where
      getKey :: Entry k v -> k
      getKey = fst . getEntry

      getFromInsertResult :: Ord k => InsertResult (MinMax k) (Entry k v) -> Tree (MinMax k) (Entry k v)
      getFromInsertResult (Inserted t) = t
      getFromInsertResult (Split l r) = node2 l r

      insert' :: Ord k => Entry k v -> Tree (MinMax k) (Entry k v) -> InsertResult (MinMax k) (Entry k v)
      insert' e Empty = Inserted (leaf e)
      insert' e (Leaf e')
        | getKey e <= getKey e' = Inserted (node2 (leaf e) (leaf e'))
        | otherwise = Inserted (node2 (leaf e') (leaf e))

      insert' e node@(Node2 m l r)
        | isTerminal node =
          if getKey e < fromJust (getMin (fst (getMinMax m))) then
            Inserted (node3 (leaf e) l r)
          else if getKey e > fromJust (getMax (snd (getMinMax m))) then
            Inserted (node3 l r (leaf e))
          else
            Inserted (node3 l (leaf e) r)
        | getKey e < fromJust (getMin (fst (getMinMax m))) = case insert' e l of
            Inserted newL -> Inserted (node2 newL r)
            Split newL newR -> Inserted (node3 newL newR r)
        | otherwise = case insert' e r of
            Inserted newR -> Inserted (node2 l newR)
            Split newL newR -> Inserted (node3 l newL newR)


      insert' e node@(Node3 m l m' r)
        | isTerminal node =
          if getKey e < fromJust (getMin (fst (getMinMax m))) then
            Split (node2 (leaf e) l) (node2 m' r)
          else if getKey e > fromJust (getMax (snd (getMinMax m))) then
            Split (node2 l m') (node2 r (leaf e))
          else if getKey e >= fromJust (getMin (fst (getMinMax m))) && getKey e < (fst . fromJust) (getEntryFromLeaf m') then
            Split (node2 l (leaf e)) (node2 m' r)
          else
            Split (node2 l m') (node2 (leaf e) r)
        | getKey e < fromJust (getMin (fst (getMinMax (measure l)))) = case insert' e l of
            Inserted newL -> Inserted (node3 newL m' r)
            Split newL newR -> Split (node2 newL newR) (node2 m' r)
        | getKey e >= fromJust (getMin (fst (getMinMax (measure l)))) && getKey e < fromJust (getMin (fst (getMinMax (measure m')))) = case insert' e m' of
            Inserted newM -> Inserted (node3 l newM r)
            Split newL newR -> Split (node2 l newL) (node2 newR r)
        | otherwise = case insert' e r of
            Inserted newR -> Inserted (node3 l m' newR)
            Split newM newR -> Split (node2 l m') (node2 newM newR)
  extractMin = getFromMaybeRemoveResult . extractMinFromTree . getTree where
    extractMinFromTree :: Ord k => Tree (MinMax k) (Entry k v) -> Maybe (v, RemoveResult (MinMax k) (Entry k v))
    extractMinFromTree Empty = Nothing
    extractMinFromTree (Leaf e) = Just (snd (getEntry e), Removed Empty)
    extractMinFromTree node@(Node2 _ l r)
      | isTerminal node =
        let v = (fromJust . getValueFromLeaf) l
        in Just (v, Underflow r)
      | otherwise = case extractMinFromTree l of
          Just (v, Removed newL) -> Just (v, Removed (node2 newL r))
          Just (v, Underflow newL) -> case r of
            Node2 _ l' r' -> Just (v, Underflow (node3 newL l' r'))
            Node3 _ l' m' r' -> Just (v, Removed (node2 (node2 newL l') (node2 m' r')))
            _ -> error "WTF: right child of a non-terminal Node2 should be a Node2 or Node3"
          Nothing -> case extractMinFromTree r of
            Just (v, Removed newR) -> Just (v, Removed (node2 l newR))
            Just (v, Underflow newR) -> case l of
              Node2 _ l' r' -> Just (v, Underflow (node3 l' r' newR))
              Node3 _ l' m' r' -> Just (v, Removed (node2 (node2 l' m') (node2  r' newR)))
              _ -> error "WTF: left child of a non-terminal Node2 should be a Node2 or Node3"
            Nothing -> Nothing
    extractMinFromTree node@(Node3 _ l m r)
      | isTerminal node =
        let v = (fromJust . getValueFromLeaf) l
        in Just (v, Underflow (node2 m r))
      | otherwise = case extractMinFromTree l of
          Just (v, Removed newL) -> Just (v, Removed (node3 newL m r))
          Just (v, Underflow newL) -> case m of
            Node2 _ l' r' -> Just (v, Removed (node2 (node3 newL l' r') r))
            Node3 _ l' m' r' -> Just (v, Removed (node3 (node2 newL l') (node2 m' r') r))
            _ -> error "WTF: middle child of a non-terminal Node3 should be a Node2 or Node3"
          Nothing -> case extractMinFromTree m of
            Just (v, Removed newM) -> Just (v, Removed (node3 l newM r))
            Just (v, Underflow newM) -> case l of
              Node2 _ l' r' -> Just (v, Removed (node2 (node3 l' r' newM) r))
              Node3 _ l' m' r' -> Just (v, Removed (node3 (node2 l' m') (node2 r' newM) r))
              _ -> error "WTF: right child of a non-terminal Node3 should be a Node2 or Node3"
            Nothing -> case extractMinFromTree r of
              Just (v, Removed newR) -> Just (v, Removed (node3 l m newR))
              Just (v, Underflow newR) -> case m of
                Node2 _ l' r' -> Just (v, Underflow (node2 l (node3 l' r' newR)))
                Node3 _ l' m' r' -> Just (v, Removed (node3 l (node2 l' m') (node2 r' newR)))
                _ -> error "WTF: middle child of a non-terminal Node3 should be a Node2 or Node3"
              Nothing -> Nothing

  extractMax = getFromMaybeRemoveResult . extractMaxFromTree . getTree where
    extractMaxFromTree :: Ord k => Tree (MinMax k) (Entry k v) -> Maybe (v, RemoveResult (MinMax k) (Entry k v))
    extractMaxFromTree Empty = Nothing
    extractMaxFromTree (Leaf e) = Just (snd (getEntry e), Removed Empty)
    extractMaxFromTree node@(Node2 _ l r)
      | isTerminal node =
        let v = (fromJust . getValueFromLeaf) r
        in Just (v, Underflow l)
      | otherwise = case extractMaxFromTree r of
          Just (v, Removed newR) -> Just (v, Removed (node2 l newR))
          Just (v, Underflow newR) -> case l of
            Node2 _ l' r' -> Just (v, Underflow (node3 l' r' newR))
            Node3 _ l' m' r' -> Just (v, Removed (node2 (node2 l' m') (node2 r' newR)))
            _ -> error "WTF: left child of a non-terminal Node2 should be a Node2 or Node3"
          Nothing -> case extractMaxFromTree l of
            Just (v, Removed newL) -> Just (v, Removed (node2 newL r))
            Just (v, Underflow newL) -> case r of
              Node2 _ l' r' -> Just (v, Underflow (node3 newL l' r'))
              Node3 _ l' m' r' -> Just (v, Removed (node2 (node2 newL l') (node2 m' r')))
              _ -> error "WTF: right child of a non-terminal Node2 should be a Node2 or Node3"
            Nothing -> Nothing
    extractMaxFromTree node@(Node3 _ l m r)
      | isTerminal node =
        let v = (fromJust . getValueFromLeaf) r
        in Just (v, Underflow (node2 l m))
      | otherwise = case extractMaxFromTree r of
          Just (v, Removed newR) -> Just (v, Removed (node3 l m newR))
          Just (v, Underflow newR) -> case m of
            Node2 _ l' r' -> Just (v, Removed (node2 l (node3 l' r' newR)))
            Node3 _ l' m' r' -> Just (v, Removed (node3 l (node2 l' m') (node2 r' newR)))
            _ -> error "WTF: middle child of a non-terminal Node3 should be a Node2 or Node3"
          Nothing -> case extractMaxFromTree m of
            Just (v, Removed newM) -> Just (v, Removed (node3 l newM r))
            Just (v, Underflow newM) -> case l of
              Node2 _ l' r' -> Just (v, Removed (node2 (node3 l' r' newM) r))
              Node3 _ l' m' r' -> Just (v, Removed (node3 (node2 l' m') (node2 r' newM) r))
              _ -> error "WTF: left child of a non-terminal Node3 should be a Node2 or Node3"
            Nothing -> case extractMaxFromTree l of
              Just (v, Removed newL) -> Just (v, Removed (node3 newL m r))
              Just (v, Underflow newL) -> case m of
                Node2 _ l' r' -> Just (v, Underflow (node2 (node3 newL l' r') r))
                Node3 _ l' m' r' -> Just (v, Removed (node3 (node2 newL l') (node2 m' r') r))
                _ -> error "WTF: middle child of a non-terminal Node3 should be a Node2 or Node3"
              Nothing -> Nothing
