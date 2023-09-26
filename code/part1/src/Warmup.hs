module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...


type Pos = (Int, Int)
data Direction = North | South | East | West

-- Function to move the Pos in a Direction
move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1, y)

-- Function to return the net result of performing moves of a sequence
moves :: [Direction] -> Pos -> Pos
moves [] p = p
moves (h : t) p = moves t (move h p) 

data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord) 

-- Function to return the result of adding natural numbers
add :: Nat -> Nat -> Nat
add Zero Zero = Zero
add Zero x = x
add x Zero = x
add (Succ x) y = Succ (add x y)

-- Function to return the result of multiplying natural numbers
mult :: Nat -> Nat -> Nat
mult Zero Zero = Zero
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ Zero) y = y
mult x (Succ Zero) = x
mult (Succ x) y = (add (mult x y) y)

-- Function to convert Nat to Integer
nat2int :: Nat -> Integer
nat2int Zero = 0
nat2int (Succ Zero) = 1
nat2int (Succ x) = 1 + nat2int x

-- Function to convert Integer to Nat
int2nat :: Integer -> Nat
int2nat 0 = Zero
int2nat 1 = Succ Zero
int2nat x = Succ (int2nat (x-1))



data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)


-- Inserts an Integer into a Tree and returns the new Tree
insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
  | x == y = (Node y left right)
  | x > y = Node y left (insert x right)
  | x < y = Node y (insert x left) right
 
-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Show, Eq)

--pinsert (Same as insert, but polymorphic)
pinsert :: (Ord a) =>  a -> PTree a -> PTree a
pinsert x PLeaf = PNode x PLeaf PLeaf
pinsert x (PNode y left right)
  | x == y = (PNode y left right)
  | x > y = PNode y left (pinsert x right)
  | x < y = PNode y (pinsert x left) right
