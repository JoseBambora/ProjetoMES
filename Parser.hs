{-
Grupo:
- PG53975 José Carvalho
- PG54097 Miguel Silva
- PG52689 José Barbosa
-}

module Parser where

import Prelude hiding ((<*>), (<$>))
import Data.Char

infixl 2 <|>
infixl 3 <*>

type Parser type_result = String -> [(type_result,String)]

symbola :: Parser Char
symbola []  = []
symbola ('a':t) = [('a',t)]
symbola _ = []


symbol :: Char -> Parser Char
symbol s [] = []
symbol s (h:t) | s == h = [(s,t)]
               | otherwise = []

satisfy :: (Char -> Bool) -> Parser Char
satisfy _ [] = []
satisfy p (h:t) | p h = [(h,t)]
                | otherwise = []

token :: String -> Parser String
token t input | t == fstpart = [(fstpart,sndpart)]
              | otherwise = []
        where 
            tokenlen = length t
            fstpart = take tokenlen input
            sndpart = drop tokenlen input

succeed :: a -> Parser a
succeed r input = [(r,input)]

(<|>) :: Parser a -> Parser a -> Parser a
(p <|> q) input = p input ++ q input

-- X -> While
--    | For
--    | while

pX :: Parser String
pX =  token "While" 
  <|> token "For" 
  <|> token "while"

-- S -> A B
-- A -> epsilon
--   | a A
-- B -> b
--   | b B
{-
(<*>) :: Parser a -> Parser b -> Parser (a,b)
(p <*> q) inp = [ ((r,r'),rst')
                | (r ,rst ) <- p inp
                , (r',rst') <- q rst
                ]
-}

(<*>) :: Parser (a -> r) -> Parser a -> Parser r
(p <*> q) inp = [ (f r,rst')
                | (f  ,rst ) <- p inp
                , (r  ,rst') <- q rst
                ]

(<$>) :: (a -> r) -> Parser a -> Parser r
(f <$> p) inp = [ (f r, rst) | (r,rst) <- p inp]

{-
As -> a
    | a A
-}

pAs  =  f <$> symbol 'a' 
    <|> g <$> symbol 'a' <*> pAs
    where 
        f x1    = 1
        g x1 x2 = 1 + x2

pA = f <$> symbol 'a' <*> symbol 'b' <*> symbol 'c' <*> symbol 'a' 
    where f x y z w = y

{-
  Int -> Sinal Digitos
  Sinal -> '+'
        |  '-'
        |  epsilon
  Digitos -> dig
           | dig Digitos
-}

pInt :: Parser Int
pInt =  f <$> pSinal <*> pDigitos <*> spaces
   where f  '-' y _ = (read ('-':y))
         f  _   y _ = read y

pSinal =   symbol '-'
      <|>  symbol '+'
      <|>  succeed '+'

pDigitos =  f <$> (satisfy isDigit)
        <|> g <$> (satisfy isDigit) <*> pDigitos
    where f d = [d]
          g d ds = d : ds


-- pInt = f <$> pSinal <*> pDigitos
--     where f x y = x:y
-- 
-- pSinal =  symbol  '-' 
--       <|> symbol  '+' 
--       <|> succeed '+'
-- 
-- pDigitos =  f <$> satisfy isDigit 
--         <|> g <$> satisfy isDigit <*> pDigitos 
--     where
--         f d = [d]
--         g d ds = d:ds
-- 
oneOrMore p =  f <$> p 
           <|> g <$> p <*> (oneOrMore p)
    where
        f x = [x]
        g x y = x:y

zeroOrMore p =  succeed []
            <|> oneOrMore p

pString = f <$> (symbol '\"') <*> (zeroOrMore (satisfy (/= '\"'))) <*> (symbol '\"')
        where
            f x y z = y

pOptional p = succeed []
          <|> f <$> p 
    where
        f x = [x] 

ex = pString "\"dw\""

pSinal' =  symbol '+'
       <|> symbol '-'

pInt' = f <$> (pOptional (pSinal')) <*> (oneOrMore (satisfy isDigit))
    where
        f a b = a ++ b

separatedBy p s =  f <$> p
               <|> g <$> p <*> s <*> (separatedBy p s)
    where
        f x = [x]
        g x _ z = x:z

followedBy p s = succeed []
              <|> f <$> p <*> s <*> (followedBy p s)
        where
            f a _ b = a : b

enclosedBy a c f = fun <$> a <*> c <*> f
        where 
            fun _ x _ = x


pListsIntHaskell = enclosedBy (symbol '[') (separatedBy (pInt') (symbol ',')) (symbol ']')

codeBlockC = enclosedBy (symbol '{') (followedBy (pInt') (symbol ';')) (symbol '}')

spaces = zeroOrMore (satisfy isSpace)

symbol' a  = (\x _ -> x) <$> symbol a  <*> spaces
token' t   = (\x _ -> x) <$> token t   <*> spaces
satisfy' p = (\x _ -> x) <$> satisfy p <*> spaces