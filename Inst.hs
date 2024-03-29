{-
Grupo:
- PG53975 José Carvalho
- PG54097 Miguel Silva
- PG52689 José Barbosa
-}

{-# LANGUAGE DeriveDataTypeable #-}
module Inst where

import Test.QuickCheck
import Parser
import Prelude hiding ((<*>), (<$>))
import Data.Char
import Exp
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)

type BlocoC = [Inst]

data Inst = Inline Exp
          | Dec String String
          | Atrib String String Exp
          | While Exp BlocoC
          | IFE Exp BlocoC BlocoC
      deriving (Eq, Data)

instance StrategicData Inst
instance Show Inst where
      show = unparserInst 0

pType :: Parser String
pType =  token' "int"
     <|> token' "char"
     <|> token' "bool"

pType' :: Parser String
pType' =  pType
      <|> succeed ""

pDec :: Parser Inst
pDec = (\x y _ -> Dec x y) <$> pType <*> pNome <*> symbol' ';'

pAtrib :: Parser Inst
pAtrib = (\t x _ y _ -> Atrib t x y) <$> pType' <*> pNome <*> symbol' '=' <*> pExp <*> symbol' ';'

pWhile :: Parser Inst
pWhile = (\_ c x -> While c x) <$> token' "while" <*> pExpPar <*> pBlocoC

pIFE :: Parser Inst
pIFE = (\_ c _ x _ y -> IFE c x y) <$> token' "if" <*> pExpPar <*> token' "then" <*> pBlocoC <*> token' "else" <*> pBlocoC

pInline :: Parser Inst
pInline = (\x _ -> Inline x) <$> pExp <*> symbol' ';'

pLine :: Parser Inst
pLine =   pDec
      <|> pAtrib
      <|> pWhile
      <|> pIFE
      <|> pInline


pBlocoC :: Parser BlocoC
pBlocoC  = enclosedBy (symbol' '{') (pBlocoC) (symbol' '}')
        <|> (\x -> [x]) <$> pLine
        <|> (\x y -> x:y) <$> pLine <*> pBlocoC


unparserInst :: Int -> Inst -> String
unparserInst n (Dec   a b)   = replicate n '\t' ++ a ++ " " ++ b ++ ";"
unparserInst n (Atrib t a b) = replicate n '\t' ++ ty ++ a ++ " = " ++ show b ++ ";"
      where
            ty = if t == "" then ""
                 else t ++ " "
unparserInst n (While c b)   = replicate n '\t' ++ "while ("  ++ show c ++ "){\n" ++ (unparserInsts (n+1) b) ++ replicate n '\t' ++ "}"
unparserInst n (IFE c i e)   = replicate n '\t' ++ "if ("     ++ show c ++ ") then {\n" ++ (unparserInsts (n+1) i) ++ replicate n '\t' ++ "} else {\n" ++ (unparserInsts (n+1) e) ++ replicate n '\t' ++ "}"
unparserInst n (Inline c)    = replicate n '\t' ++ show c ++ ";"

unparserInsts :: Int -> [Inst] -> String
unparserInsts t l = foldr (\x acc -> x ++ "\n" ++ acc)""(map (unparserInst t) l)

{-
          | Inline Exp
          | Dec String String
          | Atrib String String Exp
          | While Exp BlocoC
          | IFE Exp BlocoC BlocoC
-}

types = ["int","char"]

genInline :: Gen Inst
genInline = do exp <- genExp
               return (Inline exp)

genDec :: Gen Inst
genDec = do t <- elements types
            v <- elements vars
            return (Dec t v)

genAtrib :: Gen Inst
genAtrib = do ty <- elements types
              t <- frequency [(50,return ""),(50,return ty)]
              v <- elements vars
              exp <- genExp
              return (Atrib t v exp)

genWhile :: Int -> Gen Inst
genWhile n = do exp <- genExp
                blocoC <- genBlocoC2 (n-1)
                return (While exp blocoC)

genIFE :: Int -> Gen Inst
genIFE n = do exp <- genExp
              blocoC1 <- genBlocoC2 (n-1)
              blocoC2 <- genBlocoC2 (n-1)
              return (IFE exp blocoC1 blocoC2)

-- genBlocoCAux :: Gen Inst
-- genBlocoCAux = frequency [(300,genInline),(100,genDec),(500,genAtrib),(1,genWhile),(1,genIFE)]

genBlocoCAux :: Int -> Gen Inst
genBlocoCAux n = frequency [(300,genInline),(100,genDec),(500,genAtrib),(1*n,genWhile n),(1*n,genIFE n)]


genBlocoC2 :: Int -> Gen [Inst]
genBlocoC2 n = listOf1 (genBlocoCAux n)

genBlocoC :: Gen [Inst]
genBlocoC = genBlocoC2 1