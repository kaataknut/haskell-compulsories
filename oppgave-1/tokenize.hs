import Data.Char

tokenize :: String -> [String]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('(':xs) = "(":tokenize xs
tokenize (')':xs) = ")":tokenize xs
tokenize ('+':xs) = "+":tokenize xs
tokenize ('*':xs) = "*":tokenize xs
tokenize ('-':xs) = "-":tokenize xs
tokenize ('/':xs) = "/":tokenize xs
tokenize (';':xs) = ";":tokenize xs
tokenize (x:xs) =
  if isDigit x
  then (takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit xs))
  else tokenize xs

data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast | Lambda String Ast
  deriving (Eq, Show, Ord)

type Context = [(String, Integer)]
type Memory  = [(Integer, Integer -> Maybe Ast)]

-- Primitive helper functions for improved readibility
parseNumber::String -> Integer
parseNumber x = read x

isFunc::String -> Bool
isFunc x = x == "+" || x == "-" || x == "/" || x == "*"

evalNumber::Ast -> Integer
evalNumber (Number a) = a

-- Parse functions
parse::String -> Ast
parse s = fst $ parseBlock(tokenize s)

parseBlock::[String] -> (Ast, [String])
parseBlock xs
  | last xs == ";" = (Block [expr], [])
  | otherwise      = error "Unknown parsing error!"
  where
    (expr, rest) = parseExpr xs

parseExpr::[String] -> (Ast, [String])
parseExpr (x:xs) 
  | isDigit (head x) = (Number (parseNumber x), xs)
  | x == "("         = parseApp xs
  | otherwise        = error "Unknown expression error!"

parseApp::[String] -> (Ast, [String])
parseApp xs
  | isOp && head rest1 == ")"  = ((App (Name op) [a, b]), rest)
  | otherwise                  = error "Unknown app error"
  where
    (a, rest0) = parseExpr(xs)
    (b, rest1) = parseExpr(rest0)
    op         = rest1 !! 1
    isOp       = isFunc op
    rest       = drop 2 rest1

-- Block [App (Name "+") [App (Name "-") [Number 10,Number 2],Number 4]]
eval::Ast -> Context -> Memory -> (Ast, Context, Memory)
eval (Number a) context memory = (Number a, context, memory)
eval (Block [expr]) context memory = eval expr context memory

simpleEval::Ast -> Ast
simpleEval (Number a)             = Number a
simpleEval (Block [expr])         = simpleEval expr
simpleEval (App (Name op) [a,b])
  | op == "+" = Number(evalNumber e1 + evalNumber e2)
  | op == "-" = Number(evalNumber e1 - evalNumber e2)
  | op == "*" = Number(evalNumber e1 * evalNumber e2)
  | op == "/" = Number(evalNumber e1 `div` evalNumber e2)
  | otherwise = error "Unsupporteed operator!"
  where
    e1 = simpleEval a
    e2 = simpleEval b

run::String -> Ast
run s = simpleEval (parse s)



















