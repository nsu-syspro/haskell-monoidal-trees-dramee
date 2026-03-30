{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task4.PQueue where

import Common.PriorityQueue

import Task1 (Measured(..), MinMax(..), getMin, getMinMax, getMax)
import Task4.Tree (Tree(Empty), split, (><), Split (..), splitTree )
import Common.MonoidalTree (MonoidalTree ((|>)))

newtype PQueue k v = PQueue { getTree :: Tree (MinMax k) (Entry k v) }
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry { getEntry :: (k, v) }
  deriving (Show, Eq)

-- | Measures given entry using both minimum and maximum priority 'k'
instance Ord k => Measured (MinMax k) (Entry k v) where
  measure (Entry (k, _)) = measure k

-- * Priority queue instance


splitByMin :: Ord k => k -> PQueue k v -> (PQueue k v, PQueue k v)
splitByMin k (PQueue t) = (PQueue l, PQueue r) where
  (l, r) = split (\m -> case getMin (fst (getMinMax m)) of
    Just minK -> minK >= k
    Nothing -> False) t
  
splitByMax :: Ord k => k -> PQueue k v -> (PQueue k v, PQueue k v)
splitByMax k (PQueue t) = (PQueue l, PQueue r) where
  (l, r) = split (\m -> case getMax (snd (getMinMax m)) of
    Just maxK -> maxK < k
    Nothing -> False) t



instance PriorityQueue PQueue where
  empty = PQueue Empty
  toPriorityQueue = foldr (uncurry insert) empty
  entries = foldMap (\(Entry (k, v)) -> [(k, v)]) . getTree 
  insert k v pq = 
    let 
      (PQueue l, PQueue r) = splitByMin k pq 
      (PQueue l', PQueue r') = splitByMax k (PQueue r)
    in 
      PQueue $ l >< (l' |> Entry (k, v)) >< r'
  extractMin (PQueue q) = case splitTree isMin mempty q of
      Nothing                         -> Nothing
      Just (Split l (Entry (_, v)) r) -> Just (v, PQueue (l >< r))
      where
        isMin = (== fst (getMinMax (measure q))) . fst . getMinMax
  extractMax (PQueue q) = case splitTree isMax mempty q of
    Nothing                         -> Nothing
    Just (Split l (Entry (_, v)) r) -> Just (v, PQueue (l >< r))
    where
      isMax = (== snd (getMinMax (measure q))) . snd . getMinMax
