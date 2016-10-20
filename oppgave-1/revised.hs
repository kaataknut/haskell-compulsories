-- Imports
import Data.Char
import Data.Maybe
import Data.List

-- Data types
data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast | Lambda String Ast
  deriving (Eq, Show, Ord)


delimiters = "+-*/,;()"
tokenize :: String -> [String]
tokenize []           = [];
tokenize('=':'=':xs)  = "==":tokenize xs
tokenize('!':'=':xs)  = "!=":tokenize xs
tokenize('-':'>':xs)  = "->":tokenize xs
tokenize (x:xs)
  | isDigit x         = let (numb, rest) = span isDigit (x:xs) 
                        in (numb:tokenize rest)
  | isAlpha x         = let (word, rest) = span isAlpha (x:xs)
                        in (word:tokenize rest)
  | elem x delimiters = [x]:tokenize xs
  | otherwise         = error ("\nInvalid character ----> " ++ [x])

parse :: String -> Ast
parse = fst . parseBlock . tokenize

parseBlock :: [String] -> (Ast, [String])
parseBlock []    = (Block [], [])
parseBlock xs    = (Block (expr_1:expr_2), rest_2)
  where 
    (expr_1, ";":rest_1)      = parseExpr xs
    (Block expr_2, rest_2)    = parseBlock rest_1


parseExpr :: [String] -> (Ast, [String])
parseExpr (x:xs)
  | isDigit (head x) = (Number (read x), xs)
  | x == "("         = parseApp (x:xs)
  | otherwise        = error ("\nInvalid syntax -----> " ++ show(x:xs))

parseApp :: [String] -> (Ast, [String])
parseApp("(":xs) = ((App (Name operator) [expr_1, expr_2], drop 2 rest_2))
  where
    (expr_1, ",":rest_1) = parseExpr xs
    (expr_2, rest_2)     = parseExpr rest_1
    operator             = rest_2 !! 1