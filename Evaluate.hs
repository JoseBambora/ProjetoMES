module Evaluate where

import PicoC
import Exp
import Inst

type Inputs = [(String,Int)]




eval :: Exp -> Inputs -> Int
eval (Const i)  _ = i
eval (Bool b) _ = if b then 1 else 0
eval (Var v)    l = (snd. head . filter (\(x,_) -> x == v)) l
eval (Add e d)  l = eval e l + eval d l
eval (Sub e d)  l = eval e l - eval d l
eval (Mult e d) l = eval e l * eval d l
eval (Div e d)  l = div (eval e l) (eval d l)
eval (Neg e)    l = (eval e l) * (-1)
eval (Not e)    l = if ((eval e l) /= 0) then 0 else 1
eval (Equal e d)     l = if (eval e l == eval d l) then 1 else 0
eval (Dif e d)       l = if (eval e l /= eval d l) then 1 else 0
eval (Greater e d)   l = if (eval e l > eval d l) then 1 else 0
eval (GreaterEq e d) l = if (eval e l >= eval d l) then 1 else 0
eval (Less e d)      l = if (eval e l < eval d l) then 1 else 0
eval (LessEq e d)    l = if (eval e l <= eval d l) then 1 else 0


{-
Inline Exp
          | Dec String String
          | Atrib String String Exp
          | While Exp BlocoC
          | IFE Exp BlocoC BlocoC
          | Return Exp
-}

takeout :: String -> Inputs -> Inputs
takeout s = filter (\(x,_) -> x /= s)

evalInst :: Inst -> Inputs -> (Int,Bool, Inputs)
evalInst (Inline exp)  input = (eval exp input, False, input)
evalInst (Return exp)  input = (eval exp input, True, input)
evalInst (While e b)   input = if ((eval e input) /= 0) then 
                                    if (end) then (res,end,ni)
                                    else evalInst (While e b) ni 
                               else (0,False,input)
    where
        (res,end,ni) = evalInsts b input
evalInst (IFE e b1 b2) input = if ((eval e input) /= 0) then evalInsts b1 input  else evalInsts b2 input
evalInst (Dec _ v)     input = (0,False,(v,0):(takeout v input))
evalInst (Atrib _ v e) input = (0,False,(v, eval e input):(takeout v input))

evalInsts :: [Inst] -> Inputs -> (Int,Bool, Inputs)
evalInsts [] l = (0,False,l)
evalInsts (h:t) i | end = (res,True,i)
                  | otherwise = evalInsts t inputs
    where 
        (res,end,inputs) = evalInst h i

evaluate :: PicoC -> Inputs -> Int
evaluate (PicoC insts) input = res
    where
        (res,_,_) = evalInsts insts input


tester1 = PicoC [(IFE (Const 1) [(Return (Var "a"))] [(Return (Const 30))])]
tester2 = PicoC [(IFE (Const 0) [(Return (Bool False))] [(Return (Bool True))])]
tester3 = PicoC [(IFE (Not (Const 0)) [(Return (Bool False))] [(Return (Bool True))])]
tester4 = PicoC [Atrib "" "b" (Add (Var "b") (Const 2)), Return (Var "b")]
tester5 = PicoC [(While (Var "a") [Atrib "" "b" (Add (Var "b") (Const 2)), Atrib "" "a" (Sub (Var "a") (Const 1))]), Return (Var "b")]
