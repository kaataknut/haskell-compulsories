-- Knut Ringheim Lunde
-- Student at Høgskulen i Bergen
-- Student number: H142574
-- GitHub repo for this assignment if interested: https://github.com/kaataknut/haskell-compulsory-1

{-
TASKS
Have solved the following tasks: 1,2,3,4 and 5

ERROR HANDLING
  - Error message if the statement does not end with a semicolon
  - Syntax error if not following the correct grammar for App, Case, Bool and Expr
  - Error message if trying to name a variable a reserved word
  - Error message if not suppling approved func operator (+, -, /, *) for App expression
  - Error message if not suppling approved equals operator (==, !=, >, <) for Bool expression
  - Error message if trying to access variable which is not defined
  - And some generic error messages for some sort of syntax error
    1. Ending an expression unsatisfactory will cause a "No tokens to parse" error
    2. Some other error will give a "Syntax error! " followed by the rest of the tokenizer
-}

module Oblig12016 where

-- Imports
import Data.Char
import Data.Maybe

-- Data and types
data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast | Lambda String Ast
  deriving (Eq, Show, Ord)

-- Memory
type Memory  = (Integer, Integer -> Maybe Ast)

emptyMem :: Memory
emptyMem = (0, \ _ -> Nothing)

lookupMem :: Memory -> Integer -> Maybe Ast
lookupMem (_, func) key = func key

-- nextPtr     --> Current memory address
-- nextPtr + 1 --> Next memory address
addToMem :: Memory -> Ast -> (Integer, Memory)
addToMem (nextPtr, func) ast = (nextPtr, (nextPtr + 1, \ptr -> if (ptr == nextPtr) 
                                                     then Just ast 
                                                     else lookupMem (nextPtr, func) ptr))

-- Context
newtype Context = Context (String -> Maybe Integer)
instance Show Context where
  show _ = ""

emptyCtx :: Context
emptyCtx = Context (const Nothing)

lookupCtx :: Context -> String -> Maybe Integer
lookupCtx (Context func) key = func key

-- name     --> Variable name
-- ptr      --> Memory address
addToCtx :: Context -> String -> Integer -> Context
addToCtx (Context ctx) name ptr = Context (\str -> if name == str
                                                   then Just ptr
                                                   else ctx str)

-- Tokenizer
tokenize :: String -> [String]
tokenize []           = []
tokenize (' ':xs)     = tokenize xs
tokenize('-':'>':xs)  = "->":tokenize xs
tokenize ('!':'=':xs) = "!=":tokenize xs
tokenize ('=':'=':xs) = "==":tokenize xs
tokenize ('(':xs)     = "(":tokenize xs
tokenize (')':xs)     = ")":tokenize xs
tokenize ('+':xs)     = "+":tokenize xs
tokenize ('*':xs)     = "*":tokenize xs
tokenize ('-':xs)     = "-":tokenize xs
tokenize ('/':xs)     = "/":tokenize xs
tokenize (';':xs)     = ";":tokenize xs
tokenize (',':xs)     = ",":tokenize xs
tokenize ('>':xs)     = ">":tokenize xs
tokenize ('<':xs)     = "<":tokenize xs
tokenize (x:xs)
  | isDigit x = (takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit xs))
  | isAlpha x = (takeWhile isAlpha (x:xs) : tokenize (dropWhile isAlpha xs))
  | otherwise = tokenize xs

-- Helper functions for improved readibility
isFunc::String -> Bool
isFunc op = op == "+" || op == "-" || op == "/" || op == "*"

isEqFunc::String -> Bool
isEqFunc eq = eq == "!=" || eq == "<" || eq == ">" || eq == "=="

evalNumber::Ast -> Integer
evalNumber (Number a) = a

isReservedWord::String -> Bool
isReservedWord str = str == "set" || str == "lambda" || str == "case" || str == "otherwise"

first::(a, b, c) -> a
first (a, _, _) = a

-- Parse functions that use parseBlock and tokenize
parse::String -> Ast
parse s = fst $ parseBlock(tokenize s)

-- Checks if the last statement ends with semicolon
-- Returns Block [expr_1, expr_2, ..., exrp_n]
parseBlock::[String] -> (Ast, [String])
parseBlock [] = (Block [], [])
parseBlock xs
  | last xs == ";" = (Block (first:next), rest)
  | otherwise      = error "Syntax error! Each statement must end with semicolon (;)"
  where
    (first, ";":tokens) = parseExpr xs
    (Block next, rest)  = parseBlock tokens

-- Ignores "," and "->"
parseExpr::[String] -> (Ast, [String])
parseExpr []         = error ("parseExpr: No tokens to parse!")
parseExpr (x:xs) 
  | isDigit (head x) = (Number (read x), xs)
  | x == "("         = parseApp xs
  | x == "case"      = parseCase xs
  | x == "set"       = parseSet xs
  | isAlpha (head x) = parseVar (x:xs)
  | x == "," 
    || x == "->"     = parseExpr xs
  | otherwise        = error ("parseExpr: Syntax error! " ++ show(x:xs))

-- An App has to end with a closing parenthesis as well as have a operator -> (+, -, /, *)
parseApp::[String] -> (Ast, [String])
parseApp xs
  | not(isOp)                  = error ("Missing or unsupported func operator! " ++ show(op))
  | isOp && head rest2 == ")"  = ((App (Name op) [a, b]), rest)
  | otherwise                  = error ("parseApp: Syntax error! " ++ show(xs))
  where
    (a, rest1) = parseExpr(xs)
    (b, rest2) = parseExpr(rest1)
    op         = rest2 !! 1
    isOp       = isFunc op
    rest       = drop 2 rest2

-- If otherwise use Case Default-ast
parseCase::[String] -> (Ast, [String])
parseCase []              = error ("parseCase: No tokens to parse!") 
parseCase("otherwise":xs) = (Case Default [fst $ parseExpr xs], snd $ parseExpr xs)
parseCase ("(":xs)        = (Case astBool [exprTrue, exprAfter], rest3)
  where
    (astBool, rest1)   = parseBool xs
    (exprTrue, rest2)  = parseExpr rest1
    (exprAfter, rest3) = parseExpr rest2
parseCase xs              = error ("parseCase: Syntax error! " ++ show(xs))

parseBool::[String] -> (Ast, [String])
parseBool []                  = error ("parseBool: No tokens to parse!")
parseBool (xs)
  | not(isEq)                 = error ("Missing or unsupported equals operator! " ++ show(eq))
  | isEq && head rest2 == ")" = (Bool (Name eq) e1 e2 , rest)
  | otherwise                 = error ("parseBool: Syntax error!" ++ show(xs))
  where
    (e1, rest1) = parseExpr xs
    (e2, rest2) = parseExpr rest1
    eq          = rest2 !! 1
    isEq        = isEqFunc eq
    rest        = drop 2 rest2

parseSet::[String] -> (Ast, [String])
parseSet[]          = error ("parseSet: No tokens to parse")
parseSet (x:xs) 
  | isReservedWord x = error ("Variable name cannot be: " ++ show(x))
  | otherwise        = (Set x expr, rest)
  where 
    (expr, rest) = parseExpr xs

parseVar::[String] -> (Ast, [String])
parseVar (x:xs) = (Name x, xs)

-- Eval function that evaluates an Ast
eval::Ast -> Context -> Memory -> (Ast, Context, Memory)
eval (Block [expr]) ctx mem         = eval expr ctx mem
eval (Block (first:next)) ctx mem   = eval (Block next) ctx2 mem2
  where
    (_, ctx2, mem2) = eval (Block [first]) ctx mem

-- Evaluate Set patterns
eval (Set str expr) ctx mem         = eval ast ctx3 mem3
  where
    (ast, ctx2, mem2) = eval expr ctx mem -- Evaluate the expression the variable to being set to
    (ptr1, mem3)    = addToMem mem2 ast   -- Add it to the memory
    ctx3            = addToCtx ctx2 str ptr1 -- Add the variable to the context with the ptr to the address

-- Evaluate a variable
eval (Name str) ctx mem
  | isNothing adr                   = error "Variable is not defined!"
  | otherwise                       = ((fromJust (lookupMem mem (fromJust adr))), ctx, mem)
  where
    adr   = lookupCtx ctx str

-- Evaluate App patterns
eval (App (Name op) [a,b]) ctx mem
  | op == "+"                       = (Number(e1 + e2), ctx2, mem2)
  | op == "-"                       = (Number(e1 - e2), ctx2, mem2)
  | op == "*"                       = (Number(e1 * e2), ctx2, mem2)
  | op == "/"                       = (Number(e1 `div` e2), ctx2, mem2)
  where
    (ast1, ctx1, mem1) = eval a ctx mem
    (ast2, ctx2, mem2) = eval b ctx1 mem1
    e1                 = evalNumber (ast1)
    e2                 = evalNumber (ast2)

-- Evaluate Cases
eval (Case (Bool (Name eq) a b) [exprT, other]) ctx mem
  | eq == "==" && (e1 == e2)        = eval exprT ctx2 mem2
  | eq == ">"  && (e1 > e2)         = eval exprT ctx2 mem2
  | eq == "<"  && (e1 < e2)         = eval exprT ctx2 mem2
  | eq == "!=" && (e1 /= e2)        = eval exprT ctx2 mem2
  | otherwise                       = eval other ctx2 mem2
  where
    (ast1, ctx1, mem1) = eval a ctx mem
    (ast2, ctx2, mem2) = eval b ctx1 mem1
    e1                 = evalNumber (ast1)
    e2                 = evalNumber (ast2)

-- Evaluate Default cases
eval (Case Default [expr]) ctx mem  = eval expr ctx mem

-- Evaluate Number
eval (Number a) ctx mem             = (Number a, ctx, mem)

run::String -> Ast
run s = first (eval (parse s) emptyCtx emptyMem)