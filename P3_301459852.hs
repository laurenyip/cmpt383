import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.IO

-- Data type definitions (exactly as specified)
data Type = TInt
          | TBool
          | TArr Type Type
          deriving (Eq, Ord, Read, Show)

type VarId = String

data Expr = CInt Int
          | CBool Bool
          | Var VarId
          | Plus Expr Expr
          | Minus Expr Expr
          | Equal Expr Expr
          | ITE Expr Expr Expr
          | Abs VarId Type Expr
          | App Expr Expr
          | LetIn VarId Type Expr Expr
          deriving (Eq, Ord, Read, Show)

-- Step 1: Define Env type using Map
type Env = Map.Map VarId Type

-- Step 2: Auxiliary function for arithmetic operators
typingArith :: Maybe Type -> Maybe Type -> Maybe Type
typingArith (Just TInt) (Just TInt) = Just TInt
typingArith _ _ = Nothing

-- Step 3: Auxiliary function for equality operator
typingEq :: Maybe Type -> Maybe Type -> Maybe Type
typingEq (Just TInt) (Just TInt) = Just TBool
typingEq (Just TBool) (Just TBool) = Just TBool
typingEq _ _ = Nothing

-- Step 4: Main typing function
typing :: Env -> Expr -> Maybe Type
typing env expr = case expr of
  -- T-Int: integer constants have type Int
  CInt _ -> Just TInt
  
  -- T-Bool: boolean constants have type Bool
  CBool _ -> Just TBool
  
  -- T-Ident: lookup variable in environment
  Var x -> Map.lookup x env
  
  -- T-Plus: both operands must be Int, result is Int
  Plus e1 e2 -> typingArith (typing env e1) (typing env e2)
  
  -- T-Minus: both operands must be Int, result is Int
  Minus e1 e2 -> typingArith (typing env e1) (typing env e2)
  
  -- T-Eq: both operands must be same type (Int or Bool), result is Bool
  Equal e1 e2 -> typingEq (typing env e1) (typing env e2)
  
  -- T-ITE: condition must be Bool, branches must have same type
  ITE e1 e2 e3 -> 
    case typing env e1 of
      Just TBool -> 
        case (typing env e2, typing env e3) of
          (Just t2, Just t3) -> if t2 == t3 then Just t2 else Nothing
          _ -> Nothing
      _ -> Nothing
  
  -- T-Abs: lambda x:T1.e has type T1 -> T2 if e has type T2 in extended env
  Abs x t1 e ->
    let env' = Map.insert x t1 env
    in case typing env' e of
         Just t2 -> Just (TArr t1 t2)
         Nothing -> Nothing
  
  -- T-App: application of function type T1 -> T2 to argument of type T1 gives T2
  App e1 e2 ->
    case (typing env e1, typing env e2) of
      (Just (TArr t1 t2), Just t1') -> 
        if t1 == t1' then Just t2 else Nothing
      _ -> Nothing
  
  -- T-Let: let x:T1 = e1 in e2
  -- e1 must have type T1, then type e2 in environment extended with x:T1
  LetIn x t1 e1 e2 ->
    let env' = Map.insert x t1 env
    in case typing env' e1 of
         Just t1' -> 
           if t1 == t1' 
           then typing env' e2
           else Nothing
         Nothing -> Nothing

-- Step 5: Read expression from string
readExpr :: String -> Expr
readExpr s = read s

-- Step 6: Type check and produce string output
typeCheck :: Expr -> String
typeCheck expr = 
  case typing Map.empty expr of
    Just t -> show t
    Nothing -> "Type Error"

-- Step 7: Main function for IO
main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "Usage: program <expression_file>"
    else do
      let filename = head args
      contents <- readFile filename
      let exprs = lines contents
      mapM_ (putStrLn . typeCheck . readExpr) exprs