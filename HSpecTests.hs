module Main where

import Calc
import ExprT
import Fibonacci
import Golf
import Homework1
import Homework4
import Log
import LogAnalysis
import StackVM
import Test.Hspec
import VarExprT

main :: IO ()
main = hspec $ do
    -- Week 1, Exercise 1
    describe "toDigits" $
        it "converts positive Integers to a list of digits" $ do
            toDigits 1234 `shouldBe` [1, 2, 3, 4]
            toDigits 0 `shouldBe` []
            toDigits (-17) `shouldBe` []
    describe "toDigitsRev" $
        it "converts positive Integers to a list of digits, with the digits reversed" $
            toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

    -- Week 1, Exercise 2
    describe "doubleEveryOther" $
        it "doubles every other number beginning from the right" $ do
            doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
            doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

    -- Week 1, Exercise 3
    describe "sumDigits" $
        it "calculates the sum of all digits" $
            sumDigits [16, 7, 12, 5] `shouldBe` 1 + 6 + 7 + 1 + 2 + 5

    -- Week 1, Exercise 4
    describe "validate" $
        it "indicates whether an Integer could be a valid credit card number" $ do
            validate 4012888888881881 `shouldBe` True
            validate 4012888888881882 `shouldBe` False

    -- Week 1, Exercise 5
    describe "hanoi" $
        it "given disc number and three pegs, return list of moves to be performed" $
            hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]

    -- Week 1, Exercise 6
    describe "frameStewart" $
        it "given disc number and three or more pegs, return list of moves to be performed" $
            length (frameStewart 15 "a" "b" ["c", "d"]) `shouldBe` 129

    -- Week 2, Exercise 1
    describe "parseMessage" $
        it "parses an individual line from the log file" $ do
            parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
            parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
            parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
    describe "parse" $
        it "parses an entire log file at once" $ do
            file <- readFile "week02/sample.log"
            length (parse file) `shouldBe` 11
            head (parse file) `shouldBe` LogMessage Info 6 "Completed armadillo processing"

    -- Week 2, Exercise 2
    describe "insert" $
        it "inserts a new LogMessage into an existing MessageTree" $ do
            let message = LogMessage Info 29 "la la la"
            insert message Log.Leaf `shouldBe` Log.Node Log.Leaf message Log.Leaf
            let unknown = Unknown "This is not in the right format"
            let tree = Log.Node Log.Leaf message Log.Leaf
            insert unknown tree `shouldBe` tree

    -- Week 2, Exercise 3
    let message1 = LogMessage Info 6 "Completed armadillo processing"
    let message2 = LogMessage Info 1 "Nothing to report"
    let message3 = LogMessage Info 4 "Everything normal"
    let messages = [message1, message2, message3]
    describe "build" $
        it "builds a complete MessageTree from a list of messages" $ do
            let left = Log.Node Log.Leaf message2 Log.Leaf
            let right = Log.Node Log.Leaf message1 Log.Leaf
            let tree = Log.Node left message3 right
            build messages `shouldBe` tree

    -- Week 2, Exercise 4
    describe "inOrder" $
        it "takes a MessageTree and produces a sorted list of LogMessages" $ do
            let sortedMessages = [message2, message3, message1]
            inOrder (build messages) `shouldBe` sortedMessages

    -- Week 2, Exercise 5
    describe "whatWentWrong" $
        it "takes a list of LogMessages and returns sorted messages with severity >= 50" $ do
            file <- readFile "week02/sample.log"
            let strings = [ "Way too many pickles"
                          , "Bad pickle-flange interaction detected"
                          , "Flange failed!"
                          ]
            whatWentWrong (parse file) `shouldBe` strings

    -- Week 3, Exercise 1
    describe "skips" $
        it "takes a list and outputs n lists, where the nth list contains every nth element" $ do
            skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
            skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
            skips [1] `shouldBe` [[1]]
            skips [True, False] `shouldBe` [[True, False], [False]]
            let list1 = [] :: [Integer]
            let list2 = [] :: [[Integer]]
            skips list1 `shouldBe` list2

    -- Week 3, Exercise 2
    describe "localMaxima" $
        it "finds all local maxima in the input list and returns them in order" $ do
            localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]
            localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
            localMaxima [1, 2, 3, 4, 5] `shouldBe` []

    -- Week 3, Exercise 3
    describe "histogram" $
        it "takes a list of integers between 0 and 9 and outputs a vertical histogram" $ do
            let h1 = " *        \n *        \n *   *    \n==========\n0123456789"
            histogram [1, 1, 1, 5] `shouldBe` h1
            let h2 = "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789"
            histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9] `shouldBe` h2

    -- Week 4, Exercise 1
    describe "fun1" $
        it "practices functional programming" $
            fun1 [3 .. 13] `shouldBe` 3840
    describe "fun2" $
        it "practices more functional proramming" $
            map fun2 [1 .. 10] `shouldBe` [0, 2, 40, 6, 30, 46, 234, 14, 276, 40]

    -- Week 4, Exercise 2
    describe "foldTree" $
        it "generates a balanced binary tree from a list of values" $ do
            let a = Homework4.Node 0 Homework4.Leaf 'A' Homework4.Leaf
            let b = Homework4.Node 0 Homework4.Leaf 'B' Homework4.Leaf
            let c = Homework4.Node 0 Homework4.Leaf 'C' Homework4.Leaf
            let d = Homework4.Node 0 Homework4.Leaf 'D' Homework4.Leaf
            let e = Homework4.Node 1 a 'E' Homework4.Leaf
            let f = Homework4.Node 1 b 'F' Homework4.Leaf
            let g = Homework4.Node 1 d 'G' Homework4.Leaf
            let h = Homework4.Node 2 f 'H' c
            let i = Homework4.Node 2 g 'I' e
            let j = Homework4.Node 3 i 'J' h
            foldTree "ABCDEFGHIJ" `shouldBe` j

    -- Week 4, Exercise 3
    describe "xor" $
        it "returns True iff there are an odd number of Trues" $ do
            xor [False, True, False] `shouldBe` True
            xor [False, True, False, False, True] `shouldBe` False
    describe "map'" $
        it "implements map as a fold" $
            map' (* 2) [1 .. 10] `shouldBe` [2, 4 .. 20]
    describe "myFoldl" $
        it "implements foldl using foldr" $
            myFoldl (+) 0 [1 .. 10] `shouldBe` 55

    -- Week 4, Exercise 4
    describe "cartProd" $
        it "computes the Cartesian product of two lists" $
            cartProd [1, 2] "ab" `shouldBe` [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]
    describe "sieveSundaram" $
        it "finds primes using the Sieve of Sundaram" $
            sieveSundaram 10 `shouldBe` [3, 5, 7, 11, 13, 17, 19]

    -- Week 5, Exercise 1
    describe "eval" $
        it "evaluates ExprT (expressions involving integer constants, +, and *)" $
            eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) `shouldBe` 20

    -- Week 5, Exercise 2
    describe "evalStr" $
        it "evaluates arithmetic expressions as Strings" $ do
            evalStr "(2+3)*4" `shouldBe` Just 20
            evalStr "2+3*4" `shouldBe` Just 14
            evalStr "(2+3)*" `shouldBe` Nothing

    -- Week 5, Exercise 3
    describe "Expr" $
        it "provides class methods to access Lit, Add, and Mul" $ do
            let a = mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
            let b = ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4) :: ExprT
            a `shouldBe` b

    -- Week 5, Exercise 4
    describe "Integer Expr" $
        it "works like the original calculator" $ do
            let a = mul (add (lit 2) (lit 3)) (lit 4) :: Integer
            a `shouldBe` 20
    describe "Bool Expr" $
        it "interprets literals <= 0 as False, positive as True" $ do
            let a = mul (add (lit 2) (lit 3)) (lit 4) :: Bool
            a `shouldBe` True
    describe "MinMax Expr" $
        it "takes addition to be max, multiplication to be min" $ do
            let a = mul (add (lit 2) (lit 3)) (lit 4) :: MinMax
            a `shouldBe` MinMax 3
    describe "Mod7 Expr" $
        it "does arithmetic modulo 7" $ do
            let a = mul (add (lit 2) (lit 3)) (lit 4) :: Mod7
            a `shouldBe` Mod7 6

    -- Week 5, Exercise 5
    describe "Program Expr" $
        it "generates assembly for a stack-based machine" $ do
            let a = mul (add (lit 2) (lit 3)) (lit 4) :: Program
            stackVM a `shouldBe` Right (IVal 20)
    describe "compile" $
        it "compiles strings representing arithmetic expressions into assembly" $ do
            let a = mul (add (lit 2) (lit 3)) (lit 4) :: Program
            compile "(2+3)*4" `shouldBe` Just a

    -- Week 5, Exercise 6
    describe "VarExprTi Expr" $
        it "can contain variables in arithmetic expressions" $ do
            let a = add (lit 3) (var "x") :: VarExprT
            let b = VarExprT.Add (VarExprT.Lit 3) (VarExprT.Var "x") :: VarExprT
            a `shouldBe` b
    describe "VarExprT HasVars" $
        it "looks up expression variables in associated map" $ do
            let a = withVars [("x", 6)] $ add (lit 3) (var "x")
            a `shouldBe` Just 9
            let b = withVars [("x", 6)] $ add (lit 3) (var "y")
            b `shouldBe` Nothing
            let c = withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
            c `shouldBe` Just 54

    -- Week 6, Exercise 1
    describe "fibs1" $
        it "contains infinite list of Fibonacci numbers (inefficient)" $
            take 10 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

    -- Week 6, Exercise 2
    describe "fibs2" $
        it "contains infinite list of Fibonacci numbers (O(n))" $
            take 10 fibs2 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

    -- Week 6, Exercise 3
    let ones = Cons 1 ones
    describe "streamToList" $
        it "converts Stream to infinite list" $
            take 10 (streamToList ones) `shouldBe` replicate 10 1

    -- Week 6, Exercise 4
    describe "streamRepeat" $
        it "generates Stream containing infinte copies of element" $
            take 10 (streamToList ones) `shouldBe` replicate 10 1
    describe "streamMap" $
        it "applies a function to every element of a Stream" $
            take 10 (streamToList $ streamMap (+ 1) ones) `shouldBe` replicate 10 2
    describe "streamFromSeed" $
        it "generates Stream from seed (first element) and rule" $
            take 10 (streamToList $ streamFromSeed (+ 1) 1) `shouldBe` [1 .. 10]

    -- Week 6, Exercise 5
    describe "nats" $
        it "contains infinite list of natural numbers 0, 1, 2..." $
            take 10 (streamToList nats) `shouldBe` [0 .. 9]
    describe "interleaveStreams" $
        it "alternates elements from two streams" $ do
            let twos = streamRepeat 2
            let onesTwos = interleaveStreams ones twos
            take 10 (streamToList onesTwos) `shouldBe` [1, 2, 1, 2, 1, 2, 1, 2, 1, 2]
    describe "ruler" $
        it "nth element of stream is largest power of 2 evenly dividing n" $
            take 10 (streamToList ruler) `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1]
