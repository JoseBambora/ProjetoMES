{-
Grupo:
- PG53975 José Carvalho
- PG54097 Miguel Silva
- PG52689 José Barbosa
-}

module Opt where

import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Parser
import PicoC
import Exp
import Inst

instance StrategicData Int
instance StrategicData a => StrategicData [a]

ast1 :: Exp
ast1 = Mult (Sub (Div (Add (Const 2) (Const 1)) (Const 1)) (Const 0)) (Sub (Const 1) (Const 0))

ast2 :: Exp
ast2 = Mult (Add (Const 3) (Const 0)) (Add (Const 5) (Const 0))

ast3 :: Exp
ast3 = Add (Add (Neg (Const 4)) (Const 4)) (Const 5)

ast4 :: Exp
ast4 = Add (Add ast1 ast2) (ast3)

optFinal :: Exp -> Exp
optFinal input = fromZipper res
    where
        et1 = toZipper input
        step1 = failTP `adhocTP` optExp
        Just res = applyTP (innermost step1) et1

optExp :: Exp -> Maybe Exp
optExp (Add e (Const 0)) = Just e
optExp (Add (Const 0) e) = Just e
optExp (Add (Const a) (Const b)) = Just (Const (a+b))
optExp (Sub e (Const 0)) = Just e
optExp (Sub (Const 0) e) = Just e
optExp (Sub (Const a) (Const b)) = Just (Const (a-b))
optExp (Neg (Const a)) = Just (Const (-a))
optExp (Mult e (Const 0)) = Just (Const 0)
optExp (Mult (Const 0) e) = Just (Const 0)
optExp (Mult e (Const 1)) = Just e
optExp (Mult (Const 1) e) = Just e
optExp (Mult (Const a) (Const b)) = Just (Const (a*b))
optExp (Div e (Const 1)) = Just e
optExp (Div (Const a) (Const b)) = Just (Const (div a b))
optExp (Div (Const 0) _)      = Just (Const 0)
optExp (Equal (Bool True) e)  = Just (e)
optExp (Equal (Bool False) e) = Just (Not e)
optExp (Equal e (Bool True))  = Just (e)
optExp (Equal e (Bool False)) = Just (Not e)
optExp (Dif (Bool True) e)  = Just (Not e)
optExp (Dif (Bool False) e) = Just (e)
optExp (Dif e (Bool True))  = Just (Not e)
optExp (Dif e (Bool False)) = Just (e)
optExp e = Nothing