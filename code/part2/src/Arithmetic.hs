-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

-- This function uses recursion for showing expressions in a 
-- readable way for humans. 
showExp :: Exp -> String
showExp (Cst x) = show x 
showExp (Add x y) = "(" ++ (showExp x) ++ ")" ++ "+" ++ "(" ++ (showExp y) ++ ")"
showExp (Sub x y) = "(" ++ (showExp x) ++ ")" ++ "-" ++ "(" ++ (showExp y) ++ ")"
showExp (Mul x y) = "(" ++ (showExp x) ++ ")" ++ "*" ++ "(" ++ (showExp y) ++ ")"
showExp (Div x y) = "(" ++ (showExp x) ++ ")" ++ "`div`" ++ "(" ++ (showExp y) ++ ")"
showExp (Pow x y) = "(" ++ (showExp x) ++ ")" ++ "^" ++ "(" ++ (showExp y) ++ ")"
showExp _ = error "Please use one of the arithmetic expressions in Definitions.hs"

-- This function calculates expressions 
evalSimple :: Exp -> Integer
evalSimple (Cst x) = x  
evalSimple (Add x y) = (evalSimple x) + (evalSimple y)
evalSimple (Sub x y) = (evalSimple x) - (evalSimple y)
evalSimple (Mul x y) = (evalSimple x) * (evalSimple y) 
evalSimple (Div x y) = (evalSimple x) `div` (evalSimple y)
evalSimple (Pow x y) = (evalSimple x) ^ (evalSimple y)
evalSimple _ = error "Please use one of the arithmetic expressions in Definitions.hs"


-- This function binds a variable to a value in an environment
extendEnv :: VName -> Integer -> Env -> Env
extendEnv x y z = \v -> if v == x then Just y else z v 

-- As evalSimple this function calculates expressions 
-- but this function can calculate more expressions than 
-- evalSimple. It also evaluates the expressions in a 
-- given environment
evalFull :: Exp -> Env -> Integer
evalFull (Cst x) _ = x  
evalFull (Add x y) z = (evalFull x z) + (evalFull y z)
evalFull (Sub x y) z = (evalFull x z) - (evalFull y z)
evalFull (Mul x y) z = (evalFull x z) * (evalFull y z) 
evalFull (Div x y) z = (evalFull x z) `div` (evalFull y z)
evalFull (Pow x (Cst y)) z | y < 0 = error "Can't raise to the power of negative number"
evalFull (Pow x y) z = (evalFull x z) ^ (evalFull y z)
evalFull (If e1 e2 e3) z = if (evalFull e1 z) == 0 then (evalFull e3 z) else (evalFull e2 z)
evalFull (Var x) z = case (z x) of Nothing -> error "No current value in environment"
                                   Just y -> y
evalFull (Let e1 e2 e3) z = evalFull e3 (extendEnv e1 (evalFull e2 z) z) 
evalFull (Sum v e1 e2 e3) z = if ((evalFull e1 z) > (evalFull e2 z)) 
                                 then 0 
                                  else evalFull (Add e3 (Sum v (Cst (evalFull (Add (Cst 1) e1) z)) e2 e3)) (extendEnv v (evalFull e1 z) z)  


-- This function should evaluate expressions in a given environment
-- but instead of aborting when encountering an error, it returns 
-- an error value, so the process does not abort. 
evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst x) _ = Right x
evalErr (Add x y) z = do
  case evalErr x z of
    Left err -> Left err
    Right x -> case evalErr y z of 
                Left err -> Left err
                Right x1 -> evalErr (Cst (x + x1)) z
evalErr (Sub x y) z = do
  case evalErr x z of
    Left err -> Left err
    Right x -> case evalErr y z of 
                Left err -> Left err
                Right x1 -> evalErr (Cst (x - x1)) z
evalErr (Mul x y) z = do
  case evalErr x z of
    Left err -> Left err
    Right x -> case evalErr y z of 
                Left err -> Left err
                Right x1 -> evalErr (Cst (x * x1)) z
evalErr (Div x (Cst 0)) z = Left EDivZero 
evalErr (Div x y) z = do 
  case evalErr x z of
    Left err -> Left err
    Right x -> case evalErr y z of 
                Left err -> Left err
                Right x1 -> evalErr (Cst (x `div` x1)) z
evalErr (Pow x (Cst y)) z | y < 0 = Left ENegPower
                          | y == 0 = Right 1
evalErr (Pow x y) z = do 
  case evalErr x z of
    Left err -> Left err
    Right x -> case evalErr y z of 
                Left err -> Left err
                Right x1 -> evalErr (Cst (x ^ x1)) z  
evalErr (If e1 e2 e3) z = if (evalErr e1 z) == Right 0 then (evalErr e3 z) else (evalErr e2 z)
evalErr (Var x) z = case (z x) of Nothing -> Left (EBadVar x)
                                  Just y -> Right y
evalErr (Let e1 e2 e3) z = do
  case evalErr e2 z of 
    Left err -> Left err 
    Right e2 -> evalErr e3 (extendEnv e1 e2 z) 
evalErr (Sum v e1 e2 e3) z = do
  eval1 <- evalErr e1 z
  eval2 <- evalErr e2 z
  if eval1 > eval2 then do
    return 0
    else (evalErr (Sum v (Cst (evalFull (Add (Cst 1) (Cst eval1)) z)) (Cst eval2) e3) (extendEnv v (evalFull (Cst eval1) z) z))
-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
