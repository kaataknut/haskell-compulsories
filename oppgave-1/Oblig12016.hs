-- Test
module Oblig12016 where

-- Imports
import Data.Char

-- Data and types
data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast | Lambda String Ast
  deriving (Eq, Show, Ord)

type Context = [(String, Integer)]
type Memory  = [(Integer, Integer -> Maybe Ast)]

-- Tokenizer
tokenize :: String -> [String]
tokenize [] = []
tokenize('-':'>':xs) = "->":tokenize xs
tokenize (' ':xs) = tokenize xs
tokenize ('(':xs) = "(":tokenize xs
tokenize (')':xs) = ")":tokenize xs
tokenize ('+':xs) = "+":tokenize xs
tokenize ('*':xs) = "*":tokenize xs
tokenize ('-':xs) = "-":tokenize xs
tokenize ('/':xs) = "/":tokenize xs
tokenize (';':xs) = ";":tokenize xs
tokenize (',':xs) = ",":tokenize xs
tokenize ('!':'=':xs) = "!=":tokenize xs
tokenize ('=':'=':xs) = "==":tokenize xs
tokenize ('>':xs) = ">":tokenize xs
tokenize ('<':xs) = "<":tokenize xs
tokenize (x:xs)
  | isDigit x = (takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit xs))
  | isAlpha x = (takeWhile isAlpha (x:xs) : tokenize (dropWhile isAlpha xs))
  | otherwise = tokenize xs

-- Primitive helper functions for improved readibility
parseNumber::String -> Integer
parseNumber x = read x

isFunc::String -> Bool
isFunc x = x == "+" || x == "-" || x == "/" || x == "*"

isEqFunc::String -> Bool
isEqFunc x = x == "!=" || x == "<" || x == ">" || x == "=="

evalNumber::Ast -> Integer
evalNumber (Number a) = a

-- Parse functions
parse::String -> Ast
parse s = fst $ parseBlock(tokenize s)

parseBlock::[String] -> (Ast, [String])
parseBlock xs
  | last xs == ";" = (Block [expr], [])
  | otherwise      = error "Syntax error! Each statement must end with semicolon (;)"
  where
    (expr, rest) = parseExpr xs

parseExpr::[String] -> (Ast, [String])
parseExpr (x:xs) 
  | isDigit (head x) = (Number (parseNumber x), xs)
  | x == "("         = parseApp xs
  | x == "case"      = parseCase xs
  | x == "," 
    || x == "->"     = parseExpr xs
  | otherwise        = error "parseExpr: Syntax error!"

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

-- ["(","(","1",",","2",")","+",",","4",")","!=", "->", "1", ",", "case", "otherwise", "->", "-1"]
-- (Bool (App (Name "+") [Number 1,Number 2]) (Number 4) (Name "!="),[])
parseBool::[String] -> (Ast, [String])
parseBool (xs)
  | isEq && head rest2 == ")" = (Bool (Name eq) e1 e2 , rest)
  | otherwise                 = (Number 0, rest)
  where
    (e1, rest1) = parseExpr xs
    (e2, rest2) = parseExpr rest1
    eq          = rest2 !! 1
    isEq        = isEqFunc eq
    rest        = drop 2 rest2


-- Block [App (Name "+") [App (Name "-") [Number 10,Number 2],Number 4]]
eval::Ast -> Context -> Memory -> (Ast, Context, Memory)
eval (Number a) context memory = (Number a, context, memory)
eval (Block [expr]) context memory = eval expr context memory

simpleEval::Ast -> Ast
simpleEval (Number a)             = Number a
simpleEval (Block [expr])         = simpleEval expr
simpleEval (App (Name op) [a,b])
  | op == "+" = Number(e1 + e2)
  | op == "-" = Number(e1 - e2)
  | op == "*" = Number(e1 * e2)
  | op == "/" = Number(e1 `div` e2)
  | otherwise = error "Unsupporteed operator!"
  where
    e1 = evalNumber (simpleEval a)
    e2 = evalNumber (simpleEval b)
simpleEval (Case (Bool (Name eq) a b) [exprT, other])
  | eq == "==" && (e1 == e2)  = simpleEval exprT
  | eq == ">"  && (e1 > e2)   = simpleEval exprT
  | eq == "<"  && (e1 < e2)   = simpleEval exprT
  | eq == "!=" && (e1 /= e2)  = simpleEval exprT
  | otherwise = simpleEval other
  where
    e1 = evalNumber (simpleEval a)
    e2 = evalNumber (simpleEval b)
simpleEval (Case Default [expr]) = simpleEval expr

run::String -> Ast
run s = simpleEval (parse s)



















