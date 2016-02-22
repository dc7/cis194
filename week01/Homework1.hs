module Homework1 where

-- Exercise 1

-- Since we're starting with the least significant digit, the algorithm is
-- naturally defined in reverse.

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = mapEveryOther (* 2)

-- We don't know if we're starting with the first or second number until we get
-- to the end of the list, so let's work in reverse again.

mapEveryOther :: (Integer -> Integer) -> [Integer] -> [Integer]
mapEveryOther f list = reverse $ mapEvenIndexes f (reverse list)

mapEvenIndexes :: (Integer -> Integer) -> [Integer] -> [Integer]
mapEvenIndexes f list = case list of
    (first : second : rest) -> first : f second : mapEvenIndexes f rest
    otherwise -> list

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- Exercise 4

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 source dest _ = [(source, dest)]
hanoi n source dest temp =
    hanoi (n - 1) source temp dest
    ++ (source, dest)
    : hanoi (n - 1) temp dest source

-- Exercise 6

-- We'll use the presumably optimal Frame-Stewart algorithm. Our choice of k is
-- only optimal for 4 pegs.
-- https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame.E2.80.93Stewart_algorithm

frameStewart :: Integer -> Peg -> Peg -> [Peg] -> [Move]
frameStewart 1 source dest _ = [(source, dest)]
frameStewart n source dest [temp] = hanoi n source dest temp
frameStewart n source dest (temp : temps) = 
    frameStewart k source temp (dest : temps)
    ++ frameStewart (n - k) source dest temps
    ++ frameStewart k temp dest (source : temps)
    where k = n - round (sqrt (2 * fromInteger n + 1)) + 1
