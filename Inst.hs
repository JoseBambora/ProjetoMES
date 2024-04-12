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
          | Return Exp
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

pReturn :: Parser Inst
pReturn = (\_ x _ -> Return x) <$> token' "return" <*> pExp <*> symbol' ';'

pLine :: Parser Inst
pLine =   pDec
      <|> pAtrib
      <|> pWhile
      <|> pIFE
      <|> pInline
      <|> pReturn


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

types = ["int","char","bool"]
varslista = ["a","b","c","d","e","f"]

getNVar :: [String] -> [String]
getNVar l | len == 0 = [""]
          | otherwise = r
      where
            r = filter (\x -> notElem x l) varslista
            len = length r 

genDec :: [String] -> Gen (Inst,String)
genDec vars = do t <- elements types
                 v <- elements (getNVar vars)
                 return (Dec t v,v)

genAtribDec :: String -> [String] -> Exp -> Gen (Inst,String)
genAtribDec ty vars e = do v <- elements (getNVar vars)
                           return ((Atrib ty v e),v)

genAtribNoDec :: [String] -> Exp -> Gen (Inst,String)
genAtribNoDec vars e = do v <- choose(0,length vars - 1)
                          return ((Atrib "" (vars !! v)  e),"")

genAtrib :: [String] -> Gen (Inst,String)
genAtrib vars = do ty <- elements types
                   exp <- genExp vars
                   res <- frequency [(80*d1,(genAtribNoDec vars exp)),(20*d, (genAtribDec ty vars exp))]
                   return res
      where
            d = (length varslista) - (length vars)
            d1 = (length vars)

genInline :: [String] -> Gen (Inst,String)
genInline vars = do exp <- genExp vars
                    return ((Inline exp),"")

genWhile :: Int -> [String] -> Gen (Inst,String)
genWhile n vars = do exp <- genExp vars
                     blocoC <- genBlocoC2 (n-1) vars
                     return ((While exp blocoC),"")

genIFE :: Int -> [String]  -> Gen (Inst,String)
genIFE n vars = do exp <- genExp vars
                   blocoC1 <- genBlocoC2 (n-1) vars
                   blocoC2 <- genBlocoC2 (n-1) vars
                   return ((IFE exp blocoC1 blocoC2),"")

-- genBlocoCAux :: Gen Inst
-- genBlocoCAux = frequency [(300,genInline),(100,genDec),(500,genAtrib),(1,genWhile),(1,genIFE)]

genBlocoCAux :: Int -> [String] -> Gen (Inst,String)
genBlocoCAux n vars = frequency [(300*num1,genInline vars),
                                 (50*num,genDec vars),
                                 (500*num1,genAtrib vars),
                                 (50*n*num1,genWhile n vars),
                                 (50*n*num1,genIFE n vars)]
      where
            l1 = length vars
            l2 = length varslista
            num = l2 - l1
            num1 = (l1 - 7) * (-1)

genBlocC2Vars :: String -> [String] -> [String]
genBlocC2Vars "" l = l
genBlocC2Vars h t = h:t

genBlocoC2 :: Int -> [String]-> Gen [Inst]
genBlocoC2 0 vars = do (h,_) <- genBlocoCAux 0 vars
                       return [h]
genBlocoC2 n vars = do (h,v) <- genBlocoCAux n vars
                       t <- genBlocoC2 (n-1) (genBlocC2Vars v vars)
                       return (h:t)

genBlocoC :: Gen [Inst]
genBlocoC = genBlocoC2 3 []