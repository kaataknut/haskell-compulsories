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

-- Primitive helper functions for improved readibility
parseNumber::String -> Integer
parseNumber x = read x

isFunc::String -> Bool
isFunc x = x == "+" || x == "-" || x == "/" || x == "*"

-- Parse functions
-- Returns the Ast without the [String] from parseBlock
-- ["(", "1", ",", "2", ")", "+", ";"]
parse::String -> Ast
parse s = fst $ parseBlock(tokenize s)

parseBlock::[String] -> (Ast, [String])
parseBlock xs
  | last xs == ";" = (Block [expr], [])
  | otherwise        = error "Unknown parsing error!"
  where
    (expr, rest) = parseExpr xs

-- [",", "2", ")", "+", ";"]
parseExpr::[String] -> (Ast, [String])
parseExpr (x:xs) 
  | isDigit (head x) = (Number (parseNumber x), xs)
  | x == "("         = parseApp xs
  | otherwise        = error "Unknown expression error!"

-- ["(", "1", ",", "2", ")", "+"]
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