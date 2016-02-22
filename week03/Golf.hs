module Golf where

-- Exercise 1

-- Let's focus more on using the standard libraries and writing idiomatic
-- Haskell than playing code golf.

skips :: [a] -> [[a]]
skips list = map (nth list) [1 .. length list]

nth :: [a] -> Int -> [a]
nth [] _ = []
nth list n = take 1 (drop (n - 1) list) ++ nth (drop n list) n

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : d) = [b | (a < b) && (c < b)] ++ localMaxima (b : c : d)
localMaxima _ = []

-- Exercise 3

histogram :: [Integer] -> String
histogram list = loop $ getCounts list

loop :: [Integer] -> String
loop counts
    | maximum counts == 0 = "==========\n0123456789"
    | otherwise = getRow counts ++ loop (map (\x -> if x == maximum counts then x - 1 else x) counts)

getRow :: [Integer] -> String
getRow counts = map (\x -> if x == maximum counts then '*' else ' ') counts ++ "\n"

getCounts :: [Integer] -> [Integer]
getCounts list = map (\x -> toInteger $ length $ filter (== x) list) [0 .. 9]
