module LogAnalysis where

import Log

-- Exercise 1

-- This will crash on some inputs, but we'll learn more about parsers in week
-- 10, so let's not skip ahead.

parseMessage :: String -> LogMessage
parseMessage message =
    let tokens = words message in
        case head tokens of
            "I" -> LogMessage Info (read (tokens !! 1)) (unwords (drop 2 tokens))
            "W" -> LogMessage Warning (read (tokens !! 1)) (unwords (drop 2 tokens))
            "E" -> LogMessage (Error (read (tokens !! 1))) (read (tokens !! 2)) (unwords (drop 3 tokens))
            _ -> Unknown message

parse :: String -> [LogMessage]
parse string = map parseMessage (lines string)

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert _ (Node _ (Unknown _ ) _) = error "Unknown node in tree!"
insert m@(LogMessage _ mts _) (Node left n@(LogMessage _ nts _) right)
    | mts < nts = Node (insert m left) n right
    | otherwise = Node left n (insert m right)

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ message : inOrder right

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map stringFromMessage $ inOrder $ build $ filter isSevere messages

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error code) _ _) = 50 <= code
isSevere _ = False

stringFromMessage :: LogMessage -> String
stringFromMessage (LogMessage _ _ string) = string
stringFromMessage (Unknown string) = string
