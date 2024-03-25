{-
Grupo:
- PG53975 José Carvalho
- PG54097 Miguel Silva
- PG52689 José Barbosa
-}

{-# LANGUAGE DeriveDataTypeable #-}
module PicoC where

import Test.QuickCheck
import Parser
import Prelude hiding ((<*>), (<$>))
import Data.Char
import Exp
import Inst
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)

data PicoC = PicoC [Inst]
      deriving (Eq, Data)

instance StrategicData PicoC
instance Show PicoC where
      show = unparserPicoC

instance Arbitrary PicoC where
      arbitrary = genPicoC

pPicoC :: Parser PicoC
pPicoC = ((\x -> PicoC x) <$> pBlocoC) . filter (\x -> x /= '\n')

unparserPicoC :: PicoC -> String
unparserPicoC (PicoC l) = unparserInsts 0 l

unparser = unparserPicoC
parser = findFinal . pPicoC

-- Testes:

findFinal :: [(a,String)] -> a
findFinal l =  (fst . head . filter (\(x,y) -> y == "")) l

prop :: PicoC -> Bool
prop ast = ast == (findFinal . pPicoC . unparserPicoC) ast

teste1 = (findFinal . pPicoC) "int a = 10;\nchar b;\nwhile(a > 0)\n{\n b = a;\na = 3; if (a == 3) then {k = 0;} else {k=a;} } if (b) then {b = 4 * 3;} else {z = (5 * a) / (4 + 5);}"
teste2 = (findFinal . pPicoC) "int a = 10;\nchar b;\nwhile(a - 30 < 5 + k)\n{\nb = a; a = 3; } if (b == a) then {k=3;} else {m=3;}"
teste3 = show (PicoC [Dec "int" "a",Dec "char" "b",Atrib "" "a" (Const 10),While (Greater (Var "a") (Const 0)) [Atrib "" "b" (Var "a"),Atrib "" "a" (Const 3),IFE (Equal (Var "a") (Const 3)) [Atrib "" "k" (Const 0)] [Atrib "" "k" (Var "a")]],IFE (Var "b") [Atrib "" "b" (Mult (Const 4) (Const 3))] [Atrib "" "z" (Div (Mult (Const 5) (Var "a")) (Add (Const 4) (Const 5)))]])
teste4 = (pPicoC . unparserPicoC) (PicoC [Dec "int" "a",Dec "char" "b",Atrib "" "a" (Const 10),While (Greater (Var "a") (Const 0)) [Atrib "" "b" (Var "a"),Atrib "" "a" (Const 3),IFE (Equal (Var "a") (Const 3)) [Atrib "" "k" (Const 0)] [Atrib "" "k" (Var "a")]],IFE (Var "b") [Atrib "" "b" (Mult (Const 4) (Const 3))] [Atrib "" "z" (Div (Mult (Const 5) (Var "a")) (Add (Const 4) (Const 5)))]])
teste5 = prop (PicoC [Dec "int" "a",Dec "char" "b",Atrib "" "a" (Const 10),While (Greater (Var "a") (Const 0)) [Atrib "" "b" (Var "a"),Atrib "" "a" (Const 3),IFE (Equal (Var "a") (Const 3)) [Atrib "" "k" (Const 0)] [Atrib "" "k" (Var "a")]],IFE (Var "b") [Atrib "" "b" (Mult (Const 4) (Const 3))] [Atrib "" "z" (Div (Mult (Const 5) (Var "a")) (Add (Const 4) (Const 5)))]])


genPicoC :: Gen PicoC
genPicoC = do codigo <- genBlocoC
              return (PicoC codigo)