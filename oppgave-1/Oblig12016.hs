-- Knut Ringheim Lunde
-- Student ved Høgskulen i Bergen
-- Studentnummer: H142574


-- Test
module Oblig12016 where

-- Imports
import Data.Char
import GHC.Base

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
tokenize('-':'>':xs)  = "->":tokenize xs
tokenize (' ':xs)     = tokenize xs
tokenize ('(':xs)     = "(":tokenize xs
tokenize (')':xs)     = ")":tokenize xs
tokenize ('+':xs)     = "+":tokenize xs
tokenize ('*':xs)     = "*":tokenize xs
tokenize ('-':xs)     = "-":tokenize xs
tokenize ('/':xs)     = "/":tokenize xs
tokenize (';':xs)     = ";":tokenize xs
tokenize (',':xs)     = ",":tokenize xs
tokenize ('!':'=':xs) = "!=":tokenize xs
tokenize ('=':'=':xs) = "==":tokenize xs
tokenize ('>':xs)     = ">":tokenize xs
tokenize ('<':xs)     = "<":tokenize xs
tokenize (x:xs)
  | isDigit x = (takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit xs))
  | isAlpha x = (takeWhile isAlpha (x:xs) : tokenize (dropWhile isAlpha xs))
  | otherwise = tokenize xs

-- Primitive helper functions for improved readibility
isFunc::String -> Bool
isFunc op = op == "+" || op == "-" || op == "/" || op == "*"

isEqFunc::String -> Bool
isEqFunc eq = eq == "!=" || eq == "<" || eq == ">" || eq == "=="

evalNumber::Ast -> Integer
evalNumber (Number a) = a

first::(a, b, c) -> a
first (a, _, _) = a

-- I can not seem to load the Data.Maybe for some reason
isNothing         :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

fromJust          :: Maybe a -> a
fromJust Nothing  = error "Cannot use fromJust on Nothing"
fromJust (Just x) = x

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

parseExpr::[String] -> (Ast, [String])
parseExpr (x:xs) 
  | isDigit (head x) = (Number (read x), xs)
  | x == "("         = parseApp xs
  | x == "case"      = parseCase xs
  | x == "set"       = parseSet xs
  | isAlpha (head x) = parseVar (x:xs)
  | x == "," 
    || x == "->"     = parseExpr xs
  | otherwise        = error ("parseExpr: Syntax error! " ++ show(x:xs))

parseApp::[String] -> (Ast, [String])
parseApp xs
  | isOp && head rest2 == ")"  = ((App (Name op) [a, b]), rest)
  | otherwise                  = error "parseApp: Syntax error!"
  where
    (a, rest1) = parseExpr(xs)
    (b, rest2) = parseExpr(rest1)
    op         = rest2 !! 1
    isOp       = isFunc op
    rest       = drop 2 rest2

parseCase::[String] -> (Ast, [String])
parseCase("otherwise":xs) = (Case Default [fst $ parseExpr xs], snd $ parseExpr xs)
parseCase ("(":xs)        = (Case astBool [exprTrue, exprAfter], rest3)
  where
    (astBool, rest1)     = parseBool xs
    (exprTrue, rest2)    = parseExpr rest1
    (exprAfter, rest3)   = parseExpr rest2

parseBool::[String] -> (Ast, [String])
parseBool (xs)
  | isEq && head rest2 == ")" = (Bool (Name eq) e1 e2 , rest)
  | otherwise                 = error "Syntax error!"
  where
    (e1, rest1) = parseExpr xs
    (e2, rest2) = parseExpr rest1
    eq          = rest2 !! 1
    isEq        = isEqFunc eq
    rest        = drop 2 rest2

parseSet::[String] -> (Ast, [String])
parseSet (x:xs) = (Set x expr, rest)
  where (expr, rest) = parseExpr xs

parseVar::[String] -> (Ast, [String])
parseVar (x:xs) = (Name x, xs)


-- Eval function that evaluates an Ast
eval::Ast -> Context -> Memory -> (Ast, Context, Memory)
eval (Block [expr]) ctx mem         = eval expr ctx mem
eval (Block (first:next)) ctx mem   = eval (Block next) ctx2 mem2
  where
    (_, ctx2, mem2) = eval (Block [first]) ctx mem

-- Evaluate Set patterns
eval (Set str expr) ctx mem         = eval ast ctx2 mem2
  where
    (ast, ctx, mem) = eval expr ctx mem
    (ptr1, mem2)    = addToMem mem ast 
    ctx2            = addToCtx ctx str ptr1

-- Evaluate a variable
eval (Name str) ctx mem
  | isNothing value                 = error "Variable is not defined!"
  | otherwise                       = ((fromJust value), ctx, mem)
  where
    adr   = lookupCtx ctx str
    value = lookupMem mem (fromJust adr)

-- Evaluate App patterns
eval (App (Name op) [a,b]) ctx mem
  | op == "+"                       = (Number(e1 + e2), ctx, mem)
  | op == "-"                       = (Number(e1 - e2), ctx, mem)
  | op == "*"                       = (Number(e1 * e2), ctx, mem)
  | op == "/"                       = (Number(e1 `div` e2), ctx, mem)
  | otherwise                       = error ("Unsupported operator! " ++ op)
  where
    e1 = evalNumber (first (eval a ctx mem))
    e2 = evalNumber (first (eval b ctx mem))

-- Evaluate Cases
eval (Case (Bool (Name eq) a b) [exprT, other]) ctx mem
  | eq == "==" && (e1 == e2)        = eval exprT ctx mem
  | eq == ">"  && (e1 > e2)         = eval exprT ctx mem
  | eq == "<"  && (e1 < e2)         = eval exprT ctx mem
  | eq == "!=" && (e1 /= e2)        = eval exprT ctx mem
  | not(isEqFunc eq)                = error ("Unsupported equal operator! " ++ eq)
  | otherwise                       = eval other ctx mem
  where
    e1 = evalNumber (first (eval a ctx mem))
    e2 = evalNumber (first (eval b ctx mem))

-- Evaluate Default cases
eval (Case Default [expr]) ctx mem  = eval expr ctx mem

-- Evaluate Number
eval (Number a) ctx mem             = (Number a, ctx, mem)


run::String -> Ast
run s = first (eval (parse s) emptyCtx emptyMem)


















