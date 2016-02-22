module Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : helper 0 1
    where helper x y = x + y : helper y (x + y)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons car cdr) = car : streamToList cdr

instance Show a => Show (Stream a) where
    show s = show (take 10 $ streamToList s) ++ "..."

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons car cdr) = Cons (f car) (streamMap f cdr)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f (f s))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = helper 0
    where helper x = interleaveStreams (streamRepeat x) (helper (x + 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons car cdr) next = Cons car (interleaveStreams next cdr)
