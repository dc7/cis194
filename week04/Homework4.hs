module Homework4 where

import Data.List

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 list = product $ map (subtract 2) (filter even list)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- Exercise 2

data Tree a
    = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

insertNode v Leaf = Node 0 Leaf v Leaf
insertNode v (Node h Leaf other Leaf) =
    Node (h + 1) (Node 0 Leaf v Leaf) other Leaf
insertNode v (Node h Leaf other right) =
    Node h (Node 0 Leaf v Leaf) other right
insertNode v (Node h left other Leaf) =
    Node h left other (Node 0 Leaf v Leaf)
insertNode v (Node h left@(Node lh _ _ _) other right@(Node rh _ _ _))
    | lh < rh = Node h (insertNode v left) other right
    | rh < lh = Node h left other (insertNode v right)
    | otherwise = let newleft@(Node nh _ _ _) = insertNode v left
                  in Node (if lh /= nh then h + 1 else h) newleft other right

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldl xor2 False
    where xor2 a b = case (a, b) of
            (False, False) -> False
            (False, True)  -> True
            (True,  False) -> True
            (True,  True)  -> False

map' :: (a -> a) -> [a] -> [a]
map' f = foldr (\x xs -> f x : xs) []

myFoldl :: (a -> a -> a) -> a -> [a] -> a
myFoldl f a bs = foldr (\b g x -> g (f x b)) id bs a

-- Exercise 4

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map baseToPrime $ bases \\ map pairToBase (cartProd bases bases)
    where
        baseToPrime x = 2 * x + 1
        bases = [1 .. n]
        pairToBase pair = uncurry (+) pair + 2 * uncurry (*) pair
