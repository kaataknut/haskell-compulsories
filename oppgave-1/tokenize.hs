import Data.Char

tokenize :: String -> [String]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('(':xs) = "(":tokenize xs
tokenize (')':xs) = ")":tokenize xs
tokenize (',':xs) = ",":tokenize xs
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


--parse::String -> Ast
--parse s = fst parseBlock(tokenize s)
parseNumber::String -> Integer
parseNumber x = read x

isFunc::String -> Bool
isFunc x = x == "+" || x == "-" || x == "/" || x == "*"

{-
-- ["(", "5", "4", ")", "x", ";"]
parseBlock::[String] -> (Ast, [String])
parseBlock (x:xs) =
  let (a,b) = parseExpr(x:xs) in (Block [a], b)

parseExpr::[String] -> (Ast, [String])
parseExpr (x:xs) 
  | isDigit (head x) = (Number (parseNumber x), parseExpr xs)
  | isFunc x = (Name x, xs)
  | x == "(" = parseApp x:xs
  | otherwise error "Some err"    

parseApp::[String] -> (Ast, [String])
parseApp (x:xs)
  | isOp && head r1 == ")" = ((App (Name op) [a, b]), rest)
  | otherwise = error "Some error"
where
  (a, r0) = parseExpr(x:xs)
  (b, r1) = parseExpr(r0)
  op = r1 !! 1
  isOp = isFunc op
  rest = drop 2 r1
  -}