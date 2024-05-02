module Mutations where

import Exp
import System.Random
import Inst
import Library.Ztrategic
import PicoC
import Library.StrategicData (StrategicData)

incConst :: Exp -> Maybe Exp
incConst (Const n) = Just (Const (n + 1))
incConst _         = Nothing

negBool :: Exp -> Maybe Exp
negBool (Bool b) = Just (Bool (not b))
negBool _        = Nothing

mutationHelper :: Exp -> Maybe Exp
mutationHelper (Const n) = Just (Const (n * 2))
mutationHelper (Bool b) = Just (Bool (not b))
mutationHelper (Add e1 e2) = Just (Mult e1 e2)
mutationHelper (Sub e1 e2) = Just (Equal e1 e2)
mutationHelper (Mult e1 e2) = Just (Div e1 e2)
mutationHelper (Div e1 e2) = Just (Add e1 e2)
mutationHelper (Equal e1 e2) = Just (Sub e1 e2)
mutationHelper _ = Nothing

mutationExpList :: Exp -> [Exp]
mutationExpList e | length(mut) == 0 = [e]
                  | otherwise = mut
    where 
        mut = mutations e mutationHelper


generator = mkStdGen 30

pickRandom :: [Exp] -> Exp
pickRandom l = l !! rand where
  n = length l
  (rand, _) = randomR (0,(n-1)) generator

mutationExp :: Exp -> Exp
mutationExp e = h
    where
        h = pickRandom (mutationExpList e)

mutationsInstList :: [Inst] -> [Inst]
mutationsInstList = map mutationsInst

mutationsInst :: Inst -> Inst
mutationsInst (Inline e)    = (Inline (mutationExp e))
mutationsInst (While e b)   = (While (mutationExp e) (mutationsInstList b))
mutationsInst (IFE e b1 b2) = (IFE (mutationExp e) (mutationsInstList b1) (mutationsInstList b2))
mutationsInst i = i


mutationPicoC :: PicoC -> PicoC
mutationPicoC (PicoC e) = (PicoC (mutationsInstList e))