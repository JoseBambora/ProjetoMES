{-# LANGUAGE DeriveDataTypeable #-}
module Gen where 

import Test.QuickCheck
import PicoC
import Parser
import Exp
import Inst
import Data.List

prop_parser :: PicoC -> Bool
prop_parser (PicoC c) = parser (unparser (PicoC c)) == (PicoC c)

exec_exp_aux :: (Exp,Exp,[String]) -> Bool
exec_exp_aux (x,y,z) = (exec_exp x z) && (exec_exp y z)

exec_exp :: Exp -> [String] -> Bool
exec_exp (Var s) l = length (filter (\x -> x == s) l) > 0
exec_exp (Add e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (Sub e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (Mult e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (Div e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (Equal e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (Dif e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (Less e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (LessEq e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (Greater e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (GreaterEq e1 e2) l = exec_exp_aux (e1,e2,l)
exec_exp (Not e) l = exec_exp e l
exec_exp (Neg e) l = exec_exp e l
exec_exp _ _ = True

-- Nova variavel
exec_var :: Inst -> [String] -> [String]
exec_var (Dec _ v) l = v:l
exec_var (Atrib "" _ _) l = l
exec_var (Atrib _ v _) l = v:l
exec_var _ l = l

-- Vê exp de atrib
exec_atrib :: Inst -> [String] -> Bool
exec_atrib (Atrib _ _ e) l = (exec_exp e l) 
exec_atrib _ _ = True

-- Recursivamente entra dentro dos blocos e avalia condições
exec_bloco :: Inst -> [String] -> Bool
exec_bloco (While e b) l = (exec_exp e l) && (prop_dev_varsaux b l)
exec_bloco (IFE e b1 b2) l = (exec_exp e l) && (prop_dev_varsaux b1 l) && (prop_dev_varsaux b1 l)
exec_bloco _ _ = True

prop_dev_varsaux :: [Inst] -> [String] -> Bool
prop_dev_varsaux [] _ = True
prop_dev_varsaux (x:xs) vars | ea && eb = prop_dev_varsaux xs ev
                             | otherwise = False
    where
        ea = exec_atrib x vars
        eb = exec_bloco x vars
        ev = exec_var x vars

prop_dev_vars :: PicoC -> Bool
prop_dev_vars (PicoC l) = prop_dev_varsaux l []


prop_unica_declaracao :: PicoC -> Bool
prop_unica_declaracao (PicoC insts) = length f5 == 0
     where
        f1 = map varName insts
        f2 = filter (\x -> length x > 0) f1
        f3 = (group . sort) f2
        f4 = foldl (\acc x -> (head x, length x):acc) [] f3
        f5 = filter (\(_,y) -> y > 1) f4 

varName :: Inst -> String
varName (Dec _ x) = x
varName (Atrib "" _ _) = ""
varName (Atrib _ x _) = x
varName _ = ""

-- teste
testepropvars = prop_dev_vars (parser "char e = not True; int d = e!=e; if (3==False) then {         e*e; } else {         e = -e; } char c = not False;")
