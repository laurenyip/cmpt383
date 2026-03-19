import Data.Char (isSpace, isAlpha, isAlphaNum, isLower)
import System.Environment (getArgs)
import System.IO
import Control.Applicative hiding (many, some)

-- Data type for propositions, as written in doc
data Prop = Const Bool
          | Var String
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Iff Prop Prop
          deriving (Eq, Read, Show)

-- Parser infrastructure from lectures
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

-- Functor instance
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\input -> case parse p input of
    []        -> []
    [(v,out)] -> [(f v, out)])

-- Applicative instance
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\input -> [(v, input)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P (\input -> case parse pf input of
    []       -> []
    [(f,out)] -> parse (fmap f px) out)

-- Monad instance
instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\input -> case parse p input of
    []        -> []
    [(v,out)] -> parse (f v) out)

-- Alternative instance (for choice)
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\input -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\input -> case parse p input of
    []       -> parse q input
    [(v,out)] -> [(v,out)])

-- Basic parsing primitives for later
item :: Parser Char
item = P (\input -> case input of
  []     -> []
  (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat pred = do
  x <- item
  if pred x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

-- many and some for Alternative
many :: (Alternative f, Monad f) => f a -> f [a]
many p = some p <|> pure []

some :: (Alternative f, Monad f) => f a -> f [a]
some p = do
  x <- p
  xs <- many p
  return (x:xs)

-- helper functions for later
-- Whitespace handling
space :: Parser ()
space = do
  many (sat isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Identifier parser (lowercase letter followed by alphanumeric)
ident :: Parser String
ident = do
  x <- sat isLower
  xs <- many (sat isAlphaNum)
  return (x:xs)

-- 4. Write a parser constant :: Parser Prop that can parse T and F.
constant :: Parser Prop
constant = 
  (do
    symbol "T"
    return (Const True))
  <|>
  (do
    symbol "F"
    return (Const False))

-- 5. Write a parser var :: Parser Prop that can parse variables.
var :: Parser Prop
var = do
  v <- token ident
  return (Var v)

-- 6. Write a parser formula :: Parser Prop that can parse all possible formulas in the language of G2.

-- Factor: highest precedence (parentheses, constants, variables)
factor :: Parser Prop
factor = 
  (do
    symbol "("
    f <- formula
    symbol ")"
    return f)
  <|> constant
  <|> var

-- AndTerm: handles negation (!)
andTerm :: Parser Prop
andTerm = 
  (do
    symbol "!"
    t <- andTerm
    return (Not t))
  <|> factor

-- OrTerm: handles conjunction (/\) - right associative
orTerm :: Parser Prop
orTerm = do
  a <- andTerm
  (do
    symbol "/\\"
    o <- orTerm
    return (And a o))
    <|> return a

-- ImplyTerm: handles disjunction (\/) - right associative
implyTerm :: Parser Prop
implyTerm = do
  o <- orTerm
  (do
    symbol "\\/"
    i <- implyTerm
    return (Or o i))
    <|> return o

-- IffTerm: handles implication (->) - right associative
iffTerm :: Parser Prop
iffTerm = do
  i <- implyTerm
  (do
    symbol "->"
    iff <- iffTerm
    return (Imply i iff))
    <|> return i

-- Formula: handles iff (<->) - right associative, lowest precedence
formula :: Parser Prop
formula = do
  iff <- iffTerm
  (do
    symbol "<->"
    f <- formula
    return (Iff iff f))
    <|> return iff

-- 7. Write a function parseFormula :: String -> String that takes a formula string (e.g., "x1 /\ x2") as input and generates a string as output representing the parsing result.

parseFormula :: String -> String
parseFormula input = 
  case parse (do space; f <- formula; space; return f) input of
    []           -> "Parse Error"
    [(v, rest)]  -> if all isSpace rest 
                    then show v 
                    else "Parse Error"
    _            -> "Parse Error"

-- 8. Write a main to handle IO and put everything together.
main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "Usage: program <formula_file>"
    else do
      let filename = head args
      contents <- readFile filename
      let formulas = lines contents
      mapM_ (putStrLn . parseFormula) formulas
