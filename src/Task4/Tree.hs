{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task4.Tree where

import Common.MonoidalTree

import Task1 (Measured(..))
import Data.Maybe (fromJust)
import Data.Foldable (toList)


-- * Finger tree definition

-- | Finger tree with values 'a' in leaves
-- Intermediate branches contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Single a
  | Deep m (Digit a) (Tree m (Node m a)) (Digit a)
  deriving (Show, Eq)

-- | 2-3 node of finger tree
data Node m a
  = Node2 m a a
  | Node3 m a a a
  deriving (Show, Eq)

-- | Finger tree digit
data Digit a
  = One   a
  | Two   a a
  | Three a a a
  | Four  a a a a
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance Measured m a => Measured m (Tree m a) where
  measure Empty = mempty
  measure (Single x) = measure x
  measure (Deep m _ _ _) = m

-- | Measures given node using provided measure of 'a'
instance Measured m a => Measured m (Node m a) where
  measure (Node2 m _ _) = m
  measure (Node3 m _ _ _) = m

-- | Measures given digit using provided measure of 'a'
instance Measured m a => Measured m (Digit a) where
  measure = foldMap measure
instance Foldable (Tree m) where
  foldMap _ Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Deep _ ld t rd) = foldMap f ld <> foldMap (foldMap f) t <> foldMap f rd

instance Foldable (Node m) where
  foldMap f (Node2 _ a b) = f a <> f b
  foldMap f (Node3 _ a b c) = f a <> f b <> f c

instance Foldable Digit where
  foldMap f (One a) = f a
  foldMap f (Two a b) = f a <> f b
  foldMap f (Three a b c) = f a <> f b <> f c
  foldMap f (Four a b c d) = f a <> f b <> f c <> f d

-- * Smart constructors

single :: a -> Tree m a
single = Single

node2 :: Measured m a => a -> a -> Node m a
node2 a b = Node2 (measure a <> measure b) a b

node3 :: Measured m a => a -> a -> a -> Node m a
node3 a b c = Node3 (measure a <> measure b <> measure c) a b c

deep :: forall m a. Measured m a => Digit a -> Tree m (Node m a) -> Digit a -> Tree m a
deep ld t rd = Deep (foldMap measure ld <> measure t <> foldMap measure rd) ld t rd

-- * Monoidal tree instance

instance MonoidalTree Tree where
  toTree = foldl (|>) Empty
  (<|) a Empty = single a
  (<|) a (Single b) = deep (One a) Empty (One b)
  (<|) a (Deep _ ld t rd) = case ld of
    Four a' b' c' d' -> deep (Two a a') (node3 b' c' d' <| t) rd
    _ -> deep (addLeft a ld) t rd where
      addLeft x (One a') = Two x a'
      addLeft x (Two a' b') = Three x a' b'
      addLeft x (Three a' b' c') = Four x a' b' c'
      addLeft _ (Four {}) = error "Invalid state: addLeft should not be called on a full digit"


  (|>) Empty a = single a
  (|>) (Single a) b = deep (One a) Empty (One b)
  (|>) (Deep _ ld t rd) a = case rd of
    Four a' b' c' d' -> deep ld (t |> node3 a' b' c') (Two d' a)
    _ -> deep ld t (addRight rd a) where
      addRight (One a') x = Two a' x
      addRight (Two a' b') x = Three a' b' x
      addRight (Three a' b' c') x = Four a' b' c' x
      addRight (Four {}) _ = error "Invalid state: addRight should not be called on a full digit"

-- * Utility functions

-- | Split result with left part, middle element and right part
data Split f a = Split (f a) a (f a)
  deriving (Show, Eq)

data ViewL s a = NilL | ConsL a (s a)
  deriving (Show, Eq)


listToDigit :: [a] -> Digit a
listToDigit [a] = One a
listToDigit [a, b] = Two a b
listToDigit [a, b, c] = Three a b c
listToDigit [a, b, c, d] = Four a b c d
listToDigit _ = error "Invalid state: listToDigit should not be called on more than four elements"

nodeToDigit :: Node m a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c

viewL :: Measured m a => Tree m a -> ViewL (Tree m) a
viewL Empty = NilL
viewL (Single x) = ConsL x Empty
viewL (Deep _ pr m sf) = ConsL (head' pr) (deepL (tail' pr) m sf) where
  head' (One a) = a
  head' (Two a _) = a
  head' (Three a _ _) = a
  head' (Four a _ _ _) = a

  tail' (One _) = []
  tail' (Two _ b) = [b]
  tail' (Three _ b c) = [b, c]
  tail' (Four _ b c d) = [b, c, d]


deepL :: Measured m a =>  [a] -> Tree m (Node m a) -> Digit a -> Tree m a
deepL [] m sf = case viewL m of
  NilL -> toTree sf
  ConsL a m' -> deep (nodeToDigit a) m' sf 
deepL pr m sf = deep (listToDigit pr) m sf 

data ViewR s a = NilR | ConsR (s a) a
  deriving (Show, Eq)


viewR :: Measured m a => Tree m a -> ViewR (Tree m) a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep _ pr m sf) = ConsR (deepR pr m (init' sf)) (last' sf) where
  last' (One a) = a
  last' (Two _ b) = b
  last' (Three _ _ c) = c
  last' (Four _ _ _ d) = d

  init' (One _) = []
  init' (Two a _) = [a]
  init' (Three a b _) = [a, b]
  init' (Four a b c _) = [a, b, c]

deepR :: Measured m a => Digit a -> Tree m (Node m a) -> [a] -> Tree m a
deepR pr m [] = case viewR m of
  NilR -> toTree pr
  ConsR m' a -> deep pr m' (nodeToDigit a)
deepR pr m sf = deep pr m (listToDigit sf)

splitDigit :: Measured m a => (m -> Bool) -> m -> Digit a -> Split [] a
splitDigit f m digit = splitDigit' f m (toList digit) where
  splitDigit' _ _ [] = error "Invalid state: splitDigit' should not be called on an empty digit"
  splitDigit' _ _ [a] = Split [] a []
  splitDigit' f' acc (x:xs)
    | f' i' = Split [] x xs
    | otherwise = let Split l y r = splitDigit' f i' xs in Split (x:l) y r 
    where
      i' = acc <> measure x

-- | Helper function for spliting tree based on given predicate and starting accumulator value
splitTree :: Measured m a => (m -> Bool) -> m -> Tree m a -> Maybe (Split (Tree m) a)
splitTree _ _ Empty = Nothing
splitTree _ _ (Single x) = Just (Split Empty x Empty)
splitTree f acc (Deep _ pr m sf)
  | f (foldMap measure pr <> acc) = let Split l x r = splitDigit f acc pr in Just (Split (toTree l) x (deepL r m sf))
  
  | f (foldMap measure pr <> measure m <> acc) = 
    let 
      Split ml xs mr = fromJust (splitTree f (foldMap measure pr <> acc) m)
      Split l x r = splitDigit f (foldMap measure pr <> measure ml <> acc) (nodeToDigit xs) in Just (Split (deepR pr ml l) x (deepL r mr sf)) 
        
  
  | otherwise = 
    let 
      Split l x r = splitDigit f (foldMap measure pr <> measure m <> acc) sf 
    in Just (Split (deepR pr m l) x (toTree r)) 
      
 
-- | Splits tree based on given predicate
split :: Measured m a => (m -> Bool) -> Tree m a -> (Tree m a, Tree m a)
split _ Empty = (Empty, Empty)
split f xs  = 
  if f (measure xs) then case splitTree f mempty xs of
    Just (Split l x r) -> (l, x <| r)
    Nothing -> error "Invalid state: splitTree should not return Nothing when predicate is satisfied"
  else (xs, Empty)

app3 :: Measured m a => Tree m a -> [a] -> Tree m a -> Tree m a
app3 Empty ts xs = foldr (<|) xs ts
app3 xs ts Empty = foldl (|>) xs ts
app3 (Single a) ts xs  = a <| foldr (<|) xs ts
app3 xs ts (Single a) = foldl (|>) xs ts |> a
app3 (Deep _ pr1 m1 sf1) ts (Deep _ pr2 m2 sf2) = deep pr1 (app3 m1 (nodes (toList sf1 ++ ts ++ toList pr2)) m2) sf2

nodes :: Measured m a => [a] -> [Node m a]
nodes [a, b] = [node2 a b]
nodes [a, b, c] = [node3 a b c]
nodes [a, b, c, d] = [node2 a b, node2 c d]
nodes (a:b:c:xs) = node3 a b c : nodes xs
nodes _ = error "Invalid state: nodes should not be called on an empty list"


-- | Concatenates two trees
infixr 6 ><
(><) :: Measured m a => Tree m a -> Tree m a -> Tree m a
(><) t1 = app3 t1 [] 

