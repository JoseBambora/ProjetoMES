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
refactor (PicoC input) = PicoC (map refactorInst input)

refactorInst :: Inst -> Inst
refactorInst input = fromZipper res
    where
        et1 = toZipper input
        step1 = failTP  `adhocTPSeq` refactoraux `adhocTPSeq` optExp
        Just res = applyTP (innermost step1) et1

refactoraux :: Inst -> Maybe Inst
refactoraux i | b1 || b3 = Just r3
              | otherwise = Nothing
    where 
        (r1,b1) = refactor1 i
        (r3,b3) = refactor3 r1

refactor1 :: Inst -> (Inst,Bool)
refactor1 (IFE e [(Inline (Bool True))] [(Inline (Bool False))]) = (Inline e, True)
refactor1 (IFE e [(Inline (Bool False))] [(Inline (Bool True))]) = (Inline (Not e),True)
refactor1 i  = (i,False)

refactor3 :: Inst -> (Inst,Bool)
refactor3 (IFE (Not e) b1 b2) = (IFE e b2 b1,True)
refactor3 i  = (i,False)

tester1 = (IFE (Const 0) [(Inline (Bool True))] [(Inline (Bool False))])
tester2 = (IFE (Const 0) [(Inline (Bool False))] [(Inline (Bool True))])
tester3 = (IFE (Not (Const 0)) [(Inline (Bool False))] [(Inline (Bool True))])

tester = (refactor . findFinal . pPicoC) "int a = 10;\nchar b;\nwhile(a > 0 * 30)\n{\n b = a;\na = 3; if (a == 3 + 50) then {True;} else {False;} } if (not b) then {b = 4 * 3;} else {z = (5 * a) / (4 + 5);}"
testeraux =  (While (Greater (Var "a") (Mult (Const 0) (Const 30))) [IFE (Equal (Var "a") (Add (Const 3) (Const 50))) [Inline (Bool True)] [Inline(Bool False)]])
testeraux2 =  IFE (Equal (Var "a") (Add (Const 3) (Const 50))) [Inline (Bool True)] [Inline(Bool False)]