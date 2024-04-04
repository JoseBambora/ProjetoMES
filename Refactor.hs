{-
Grupo:
- PG53975 José Carvalho
- PG54097 Miguel Silva
- PG52689 José Barbosa
-}

{-# LANGUAGE DeriveDataTypeable #-}
module Refactor where

import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Parser
import PicoC
import Exp
import Inst
import Opt

-- instance StrategicData PicoC
-- instance StrategicData a => StrategicData [a]

refactor :: PicoC -> PicoC
refactor (PicoC input) = PicoC r2
    where
        r1 = map refactorInst1 input
        r2 = refactorInst2 r1

refactorInst1 :: Inst -> Inst
refactorInst1 input = fromZipper res
    where
        et1 = toZipper input
        step1 = failTP  `adhocTPSeq` refactor1 `adhocTPSeq` refactor3 `adhocTPSeq` optExp
        Just res = applyTP (innermost step1) et1

refactorInst2 :: [Inst] -> [Inst]
refactorInst2 input = fromZipper res
    where
        et1 = toZipper input
        step1 = failTP  `adhocTPSeq` refactor2
        Just res = applyTP (innermost step1) et1


refactor1 :: Inst -> Maybe Inst
refactor1 (IFE e [(Inline (Bool True))] [(Inline (Bool False))]) = Just (Inline e)
refactor1 (IFE e [(Inline (Bool False))] [(Inline (Bool True))]) = Just (Inline (Not e))
refactor1 i  = Nothing

refactor2aux :: Inst -> [Inst]
refactor2aux (IFE (Bool True) b1 _)  = b1
refactor2aux (IFE (Bool False) _ b2) = b2
refactor2aux i = [i]

refactor2 :: [Inst] -> Maybe [Inst]
refactor2 l | r == l = Nothing
            | otherwise = Just r
    where
      r = foldr (\x acc -> (refactor2aux x) ++ acc) [] l

refactor3 :: Inst -> Maybe Inst
refactor3 (IFE (Not e) b1 b2) = Just (IFE e b2 b1)
refactor3 i  = Nothing

tester1 = (IFE (Const 0) [(Inline (Bool True))] [(Inline (Bool False))])
tester2 = (IFE (Const 0) [(Inline (Bool False))] [(Inline (Bool True))])
tester3 = (IFE (Not (Const 0)) [(Inline (Bool False))] [(Inline (Bool True))])

tester = (refactor . findFinal . pPicoC) "int a = 10;\nchar b;\nwhile(a > 0 * 30)\n{\n b = a;\na = 3; if (a == 3 + 50) then {True;} else {False;} } if (not b) then {b = 4 * 3;} else {z = (5 * a) / (4 + 5);}"
testeraux =  (While (Greater (Var "a") (Mult (Const 0) (Const 30))) [IFE (Equal (Var "a") (Add (Const 3) (Const 50))) [Inline (Bool True)] [Inline(Bool False)]])
testeraux2 =  IFE (Equal (Var "a") (Add (Const 3) (Const 50))) [Inline (Bool True)] [Inline(Bool False)]
testeraux3 =  PicoC [(While (Greater (Var "a") (Mult (Const 0) (Const 30))) [IFE (Bool True) [(Inline (Const 0))] [(Inline (Const 1))], IFE (Bool False) [(Inline (Const 2))] [(Inline (Const 3))], IFE (Equal (Var "a") (Add (Const 3) (Const 50))) [Inline (Bool True)] [Inline(Bool False)]])]
