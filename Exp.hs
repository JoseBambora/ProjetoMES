{-
Grupo:
- PG53975 José Carvalho
- PG54097 Miguel Silva
- PG52689 José Barbosa
-}

{-# LANGUAGE DeriveDataTypeable #-}
module Exp where


import Test.QuickCheck
import Parser
import Prelude hiding ((<*>), (<$>))
import Data.Char
import Data.Maybe
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)

data Exp = Const Int
         | Bool Bool
         | Var String
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Neg Exp
         | Not Exp
         | Equal Exp Exp
         | Dif Exp Exp
         | Less Exp Exp
         | LessEq Exp Exp
         | Greater Exp Exp
         | GreaterEq Exp Exp
      deriving (Eq, Data)

instance StrategicData Exp
instance Show Exp where
     show = unparserExp False

pNome' :: Parser Char
pNome' =   symbol' '_'
      <|> (satisfy' isLetter)

pNome'' :: Parser String
pNome'' = zeroOrMore (satisfy' (\c -> isLetter c || isDigit c || c == '_'))

pNome :: Parser String
pNome = (\x y -> x : y) <$> pNome' <*> pNome''

pAdd :: Parser Exp
pAdd = (\x _ y -> Add x y) <$> pExp0 <*> symbol' '+' <*> pExp

pSub :: Parser Exp
pSub = (\x _ y -> Sub x y) <$> pExp0 <*> symbol' '-' <*> pExp

pNeg :: Parser Exp
pNeg = (\_ y -> Neg y) <$> symbol' '-' <*> pExp

pMult :: Parser Exp
pMult = (\x _ y -> Mult x y) <$> pFactor <*> symbol' '*' <*> pExp

pDiv :: Parser Exp
pDiv = (\x _ y -> Div x y) <$> pFactor <*> symbol' '/' <*> pExp

pNot :: Parser Exp
pNot = (\_ x -> Not x) <$> token' "not " <*> pExp

pDif :: Parser Exp
pDif = (\x _ y -> Dif x y) <$> pExp1 <*> token' "!=" <*> pExp

pEqual :: Parser Exp
pEqual = (\x _ y -> Equal x y) <$> pExp1 <*> token' "==" <*> pExp

pLess :: Parser Exp
pLess = (\x _ y -> Less x y) <$> pExp1 <*> symbol' '<' <*> pExp

pLessEq :: Parser Exp
pLessEq = (\x _ y -> LessEq x y) <$> pExp1 <*> token' "<=" <*> pExp

pGreater :: Parser Exp
pGreater = (\x _ y -> Greater x y) <$> pExp1 <*> symbol' '>' <*> pExp

pGreaterEq :: Parser Exp
pGreaterEq = (\x _ y -> GreaterEq x y) <$> pExp1 <*> token' ">=" <*> pExp

pExpPar :: Parser Exp
pExpPar = enclosedBy (symbol' '(') (pExp) (symbol' ')')

pBool :: Parser Exp
pBool =  (\x -> Bool True)  <$> token' "True"
     <|> (\x -> Bool False) <$> token' "False"

pFactor :: Parser Exp
pFactor = f <$> pInt
       <|> pBool
       <|> g <$> pNome
       <|> z <$> symbol' '(' <*> pExp2 <*> symbol' ')'
    where
        f a = Const a
        g a = Var a
        z _ a _ = a

pExp2 :: Parser Exp
pExp2 =  pEqual
     <|> pGreaterEq
     <|> pGreater
     <|> pLessEq
     <|> pLess
     <|> pNot
     <|> pDif
     <|> pExp1

pExp1 :: Parser Exp
pExp1 =  pAdd
     <|> pSub
     <|> pNeg
     <|> pExp0

pExp0 :: Parser Exp
pExp0 =  pMult
     <|> pDiv
     <|> pFactor

pExp :: Parser Exp
pExp =  pExp2

unparserExpAux :: Exp -> Exp -> String -> String
unparserExpAux x y s = r ++ s ++ l
     where
          r = unparserExp True x
          l = unparserExp True y

unparserExpAux2 :: Bool -> String -> String
unparserExpAux2 True s = "(" ++ s ++ ")"
unparserExpAux2 _ s = s

unparserExp :: Bool -> Exp -> String
unparserExp b (Const x)        = show x
unparserExp b (Bool x)         = show x
unparserExp b (Var s)          = s
unparserExp b (Add x y)        = unparserExpAux2 b (unparserExpAux x y "+") 
unparserExp b (Sub x y)        = unparserExpAux2 b (unparserExpAux x y "-") 
unparserExp b (Mult x y)       = unparserExpAux2 b (unparserExpAux x y "*") 
unparserExp b (Div x y)        = unparserExpAux2 b (unparserExpAux x y "/") 
unparserExp b (Neg x)          = unparserExpAux2 b ("-" ++ unparserExp True x)     
unparserExp b (Not x)          = unparserExpAux2 b ("not " ++ unparserExp True x)  
unparserExp b (Dif x y)        = unparserExpAux2 b (unparserExpAux x y "!=")   
unparserExp b (Equal x y)      = unparserExpAux2 b (unparserExpAux x y "==")
unparserExp b (Less x y)       = unparserExpAux2 b (unparserExpAux x y "<" )
unparserExp b (LessEq x y)     = unparserExpAux2 b (unparserExpAux x y "<=")
unparserExp b (Greater x y)    = unparserExpAux2 b (unparserExpAux x y ">" )
unparserExp b (GreaterEq x y)  = unparserExpAux2 b (unparserExpAux x y ">=")


-- ===============================================================================


eval :: Exp -> [(String, Int)]-> Int
eval (Const i)  _ = i
eval (Var v)    l = fromJust (lookup v l)
eval (Add e d)  l = eval e l + eval d l
eval (Mult e d) l = eval e l * eval d l
eval (Sub e d)  l = eval e l - eval d l
eval (Div e d)  l = div (eval e l) (eval d l)

-- ===============================================================================

vars = ["a","b","c","d","e","f","g"]

-- Expressões aritméticas / lógicas

faux n = do e1 <- genExpaux (n-1)
            e2 <- genExpaux (n-1)
            return ((e1,e2))

fneg n = do e1 <- genExpaux (n-1)
            return (Neg e1)

fnot n = do e1 <- genExpaux (n-1)
            return (Not e1)

fadd n  = do (e1,e2) <- faux n
             return (Add e1 e2)

fmult n = do (e1,e2) <- faux n
             return (Mult e1 e2)

fsub n = do (e1,e2) <- faux n
            return (Sub e1 e2)

fdiv n = do (e1,e2) <- faux n
            return (Div e1 e2)

fequal n  = do (e1,e2) <- faux n
               return (Equal e1 e2)

fdif n  = do (e1,e2) <- faux n
             return (Dif e1 e2)

fless n  = do (e1,e2) <- faux n
              return (Less e1 e2)

flesseq n  = do (e1,e2) <- faux n
                return (LessEq e1 e2)

fgreater n  = do (e1,e2) <- faux n
                 return (Greater e1 e2)

fgreatereq n  = do (e1,e2) <- faux n
                   return (GreaterEq e1 e2)

-- Expressões singulares

fbool :: Gen Exp
fbool  = frequency [(50,return (Bool True)),(50,return (Bool False))]

fconst :: Gen Exp
fconst = do n <- choose (0,30)
            return (Const n)

fvar :: Gen Exp
fvar   = do v <- elements vars
            return (Var v)

fsingle :: Gen Exp
fsingle = frequency [(33,fbool),(33,fconst),(33,fvar)]

flogic :: Int -> Gen Exp
flogic n = frequency [(1,fequal n),(1, fdif n), (1,fless n), (1,flesseq n), (1,fgreater n), (1,fgreatereq n)]

farit :: Int -> Gen Exp
farit n = frequency [(1,fneg n),(1, fnot n), (1,fadd n), (1, fsub n), (1,fdiv n), (1,fmult n)]

genExpaux :: Int -> Gen Exp
genExpaux 0 = fsingle
genExpaux n = frequency [(50, flogic n),(50,farit n)]

genExp :: Gen Exp
genExp = genExpaux 1