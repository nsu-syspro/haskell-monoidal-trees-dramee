{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task4.Seq where

import Common.Sequence

import Task1 (Measured(..), Size(..))
import Task4.Tree
import Common.MonoidalTree (MonoidalTree(..))

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

instance Foldable Seq where
  foldMap f s = foldMap (f . getElem) (getTree s)
  -- An O(1) implementation of length is possible
  -- due to size of the tree being cached at each node
  length :: forall a. Seq a -> Int
  length = getSize . (measure @(Size a)) . getTree

-- * Sequence instance

splitAt' :: Int -> Seq a -> (Seq a, Seq a)
splitAt' n (Seq s) = (Seq left, Seq right)
  where
    (left, right) = split (Size n <) s

instance Sequence Seq where
  empty = Seq Empty
  toSequence = foldr (+|) empty
  (+|) a s = Seq $ Elem a <| getTree s
  (|+) s a = Seq $ getTree s |> Elem a
  insertAt n a s = let (l, r) = splitAt' n s in Seq $ getTree(l |+ a) >< getTree r

  removeAt n s = 
    if n < 0 
      then s 
    else let (l, r) = splitAt' n s in Seq $  getTree l >< removeFirst (getTree r) 
    where
      removeFirst :: Tree (Size a) (Elem a) -> Tree (Size a) (Elem a)
      removeFirst tree = case viewL tree of
        NilL -> Empty
        ConsL _ t -> t
  elemAt n s = 
    if n < 0 then 
      Nothing 
    else 
      let (_, r) = splitAt' n s in case viewL (getTree r) of
        NilL -> Nothing
        ConsL (Elem x) _ -> Just x
