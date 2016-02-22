{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import VarExprT
import qualified Data.Map as M

-- Exercise 1

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr string = do
    x <- parseExp ExprT.Lit ExprT.Add ExprT.Mul string
    return $ eval x

-- Exercise 3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

-- Exercise 4

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (<) 0
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 $ x `mod` 7
    add (Mod7 x) (Mod7 y) = Mod7 $ x + y `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ x * y `mod` 7

-- Exercise 5

instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

-- parseExp Lit Add Mul string
compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6

class HasVars a where
    var :: String -> a

instance Expr VarExprT where
    lit = VarExprT.Lit
    add = VarExprT.Add
    mul = VarExprT.Mul

instance HasVars VarExprT where
    var = Var

type ExprMap = M.Map String Integer -> Maybe Integer

instance HasVars ExprMap where
    var = M.lookup

instance Expr ExprMap where
    lit = return . Just
    add x y map = do
        xval <- x map
        yval <- y map
        return $ xval + yval
    mul x y map = do
        xval <- x map
        yval <- y map
        return $ xval * yval

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
