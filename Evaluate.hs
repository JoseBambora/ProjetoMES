module Evaluate where

import PicoC
import Exp
import Inst
import Debug.Trace

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
printInst (Print exp) = "print(" ++ show exp ++ ")"
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

-- trace "" (eval exp input)
evalPrint :: Exp -> Inputs -> [String] -> [String] -> (Int,Bool, Inputs, [String], [String]) 
evalPrint exp input insts prints = (p,False,input,insts++[printInst (Print exp)],prints ++ [show p])
    where
        p = eval exp input

evalDec :: String -> String -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalDec t v input insts = (0,False,(v,0):(takeout v input), insts++[printInst (Dec t v)])

evalAtrib :: String -> String -> Exp -> Inputs -> [String] -> (Int,Bool, Inputs, [String])
evalAtrib t v e input insts = (0,False,(v, eval e input):(takeout v input), insts++[printInst (Atrib t v e)])

evalCorpoWhile :: Exp -> [Inst] -> Inputs -> [String] -> [String] -> (Int,Bool, Inputs, [String], [String])
evalCorpoWhile cond bloco input insts prints | end = (res,end,ni,exec_insts, nprints)
                                             | otherwise = evalInst (While cond bloco) ni exec_insts nprints
    where
        (res,end,ni, exec_insts, nprints) = evalInsts bloco input insts prints

evalWhile :: Exp -> [Inst] -> Inputs -> [String]  -> [String] -> (Int,Bool, Inputs, [String], [String])
evalWhile cond bloco input insts prints | (evalCond cond input) = evalCorpoWhile cond bloco input new_insts prints
                                        | otherwise = (0,False,input,new_insts, prints)
    where
        new_insts = insts++[printCond cond]

evalIF :: Exp -> [Inst] -> [Inst] -> Inputs -> [String] -> [String] -> (Int,Bool, Inputs, [String], [String])
evalIF cond bloco1 bloco2 input insts prints | (evalCond cond input) = evalInsts bloco1 input new_insts prints  
                                             | otherwise = evalInsts bloco2 input new_insts prints
    where
        new_insts = insts++[printCond cond] 

noPrints :: (Int,Bool, Inputs, [String]) -> [String] -> (Int,Bool, Inputs, [String], [String])
noPrints (x,y,z,w) p = (x,y,z,w,p)

evalInst :: Inst -> Inputs -> [String] -> [String] -> (Int,Bool, Inputs, [String], [String])
evalInst (Inline exp)  input insts prints = noPrints (evalInline exp input insts) prints
evalInst (Return exp)  input insts prints = noPrints (evalReturn exp input insts) prints
evalInst (Print exp)   input insts prints = evalPrint  exp input insts prints
evalInst (Dec t v)     input insts prints = noPrints (evalDec t v input insts) prints
evalInst (Atrib t v e) input insts prints = noPrints (evalAtrib t v e input insts) prints
evalInst (While e b)   input insts prints = evalWhile e b input insts prints
evalInst (IFE e b1 b2) input insts prints = evalIF e b1 b2 input insts prints

evalInsts :: [Inst] -> Inputs -> [String] -> [String] -> (Int,Bool, Inputs, [String], [String])
evalInsts [] l insts prints = (0,False,l,insts, prints)
evalInsts (h:t) i insts prints | end = (res,True,i,exec_insts,nprints)
                               | otherwise = evalInsts t inputs exec_insts nprints
    where 
        (res,end,inputs,exec_insts,nprints) = evalInst h i insts prints

evaluate :: PicoC -> Inputs -> (Int,[String],[String])
evaluate (PicoC insts) input = (res,exec_insts,prints)
    where
        (res,_,_,exec_insts,prints) = evalInsts insts input [] []