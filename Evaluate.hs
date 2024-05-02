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

takeout :: String -> Inputs -> Inputs
takeout s = filter (\(x,_) -> x /= s)

printInst :: Inst -> String
printInst (Inline exp) = "Inst " ++ show exp
printInst (Return exp) = "Inst " ++ show (Return exp)
printInst (Dec t v) = "Inst " ++ show (Dec t v)
printInst (Atrib t v e) = "Inst " ++ show (Atrib t v e)

printCond :: Exp -> String
printCond exp = "Cond " ++ show exp

evalCond :: Exp -> Inputs -> Bool
evalCond cond input = (eval cond input) /= 0

evalInline :: Exp -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalInline exp input insts = (eval exp input, False, input, insts++[printInst (Inline exp)])

evalReturn :: Exp -> Inputs -> [String] -> (Int,Bool, Inputs, [String]) 
evalReturn exp input insts = (eval exp input, True, input, insts++[printInst (Return exp)])

evalDec :: String -> String -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalDec t v input insts = (0,False,(v,0):(takeout v input), insts++[printInst (Dec t v)])

evalAtrib :: String -> String -> Exp -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalAtrib t v e input insts = (0,False,(v, eval e input):(takeout v input), insts++[printInst (Atrib t v e)])

evalCorpoWhile :: Exp -> [Inst] -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalCorpoWhile cond bloco input insts | end = (res,end,ni,exec_insts)
                                      | otherwise = evalInst (While cond bloco) ni exec_insts
    where
        (res,end,ni, exec_insts) = evalInsts bloco input insts

evalWhile :: Exp -> [Inst] -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalWhile cond bloco input insts | (evalCond cond input) = evalCorpoWhile cond bloco input new_insts
                                 | otherwise = (0,False,input,new_insts)
    where
        new_insts = insts++[printCond cond]

evalIF :: Exp -> [Inst] -> [Inst] -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalIF cond bloco1 bloco2 input insts | (evalCond cond input) = evalInsts bloco1 input new_insts  
                                      | otherwise = evalInsts bloco2 input new_insts
    where
        new_insts = insts++[printCond cond] 

evalInst :: Inst -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalInst (Inline exp)  input insts = evalInline exp input insts
evalInst (Return exp)  input insts = evalReturn exp input insts 
evalInst (Dec t v)     input insts = evalDec t v input insts
evalInst (Atrib t v e) input insts = evalAtrib t v e input insts
evalInst (While e b)   input insts = evalWhile e b input insts
evalInst (IFE e b1 b2) input insts = evalIF e b1 b2 input insts

evalInsts :: [Inst] -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalInsts [] l insts = (0,False,l,insts)
evalInsts (h:t) i insts | end = (res,True,i,exec_insts)
                        | otherwise = evalInsts t inputs exec_insts
    where 
        (res,end,inputs,exec_insts) = evalInst h i insts

evaluate :: PicoC -> Inputs -> (Int,[String])
evaluate (PicoC insts) input = (res,exec_insts)
    where
        (res,_,_,exec_insts) = evalInsts insts input []