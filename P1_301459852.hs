-- P1_301459852 Lauren Yip
--" $env:PATH = "C:\ghcup\bin;" + $env:PATH"


-- IMPORTS
import qualified Data.Map.Strict as Map
import Data.List (nub)
import System.Environment (getArgs)
import System.IO

-- Type alias for variable identifiers
type VarId = String

-- Type alias for variable assignments
type VarAsgn = Map.Map VarId Bool

-- Data type for propositional formulas
data Prop = Const Bool
    | Var VarId
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Iff Prop Prop
    deriving (Eq, Read, Show)


--Write a function that returns a list of distinct variable names in a propositional formula.

findVarIds :: Prop -> [VarId]
findVarIds (Const _) = [] --Constants have no variables
findVarIds (Var v) = [v] --variable contributes itself
findVarIds (Not p) = findVarIds p --Recursively find in sub-formula
findVarIds (And p1 p2) = nub (findVarIds p1 ++ findVarIds p2) --combine
findVarIds (Or p1 p2) = nub (findVarIds p1 ++ findVarIds p2)
findVarIds (Imply p1 p2) = nub (findVarIds p1 ++ findVarIds p2)
findVarIds (Iff p1 p2) = nub (findVarIds p1 ++ findVarIds p2)

--Write a function that returns all possible variable assignments given a list of variable names. 
--Hint: given n variable names, there are 2^n variable assignments
--in total. You might want to use Map.empty and Map.insert for the map data structure.

genVarAsgns :: [VarId] -> [VarAsgn]
genVarAsgns [] = [Map.empty] --Base case: no variables means one empty assignment
genVarAsgns (v:vs) = 
    let restAsgns = genVarAsgns vs --Generate assignments for remaining variables
        withFalse = [Map.insert v False asgn | asgn <- restAsgns]
        withTrue = [Map.insert v True asgn | asgn <- restAsgns]
    in withFalse ++ withTrue


--Write a function eval :: Prop -> VarAsgn -> Bool that computes the truth value of a formula
--given a variable assignment.

eval :: Prop -> VarAsgn -> Bool
eval (Const b) _ = b --constants eval to themselves
eval (Var v) asgn = Map.findWithDefault False v asgn --look up variable value
eval (Not p) asgn = not (eval p asgn) --negation
eval (And p1 p2) asgn = eval p1 asgn && eval p2 asgn --conjunction
eval (Or p1 p2) asgn = eval p1 asgn || eval p2 asgn --disjunction
eval (Imply p1 p2) asgn = not (eval p1 asgn) || eval p2 asgn
eval (Iff p1 p2) asgn = eval p1 asgn == eval p2 asgn

--Write a function that returns whether a formula is satisfiable or not.

sat :: Prop -> Bool
sat prop = 
    let varIds = findVarIds prop --find all variables
        asgns = genVarAsgns varIds --generate all possible assignments
    in any (\asgn -> eval prop asgn) asgns -- check if any assignment makes it True



--Write a simple function that reads a formula string (e.g., Iff (Var "x1") (Var "x2")) 
--to its corresponding value of type Prop. Hint: look at the deriving clause of Prop.

readFormula :: String -> Prop
readFormula = read

--Write a function that takes a formula string as input and
--produces a string as input indicating whether the formula is satisfiable.

checkFormula :: String -> String
checkFormula formulaStr = 
    let prop = readFormula formulaStr
    in if sat prop then "SAT" else "UNSAT"

--Write a main function to handle IO and put everything together.

main :: IO ()
main = do
    args <- getArgs  --command line args

    if null args 
        then putStrLn "no filename"
        else do
            let filename = head args

            --read file contents
            contents <- readFile filename

            --split into lines and process each formula
            let formulaLines = lines contents

            --for each line read formula, check satisfiability, print result
            mapM_ (putStrLn . checkFormula) formulaLines
            


