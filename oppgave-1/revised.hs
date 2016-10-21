-- Imports
import Data.Char
import Data.Maybe
import Data.List

-- Data types
data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast | Lambda String Ast
  deriving (Eq, Show, Ord)


-- Helper functions
isReservedWord :: String -> Bool
isReservedWord str = elem str ["set", "lambda", "case", "otherwise"]

delimiters = "+-*/,;()."
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
  | isSpace x         = tokenize xs
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
  | x == "case"      = parseCase xs
  | x == "set"       = parseSet xs
  | isAlpha (head x) = parseVar (x:xs)
  | otherwise        = error ("\nInvalid syntax -----> " ++ show(x:xs))

parseApp :: [String] -> (Ast, [String])
parseApp("(":xs) = ((App (Name operator) [expr_1, expr_2], drop 2 rest_2))
  where
    (expr_1, ",":rest_1) = parseExpr xs
    (expr_2, rest_2)     = parseExpr rest_1
    operator             = rest_2 !! 1


parseCase :: [String] -> (Ast, [String])
parseCase("otherwise":"->":xs)  = let (expr, ".":rest)    = parseExpr xs
                                  in (Case Default [expr], rest)
parseCase("(":xs)               = let (bool, rest_1)      = parseBool xs
                                      (expr, ",":rest_2)  = parseExpr rest_1
                                      (other, rest_3)     = parseExpr rest_2
                                  in (Case bool [expr, other], rest_3)



parseBool :: [String] -> (Ast, [String])
parseBool xs = let (expr_1, ",":rest_1) = parseExpr xs
                   (expr_2, rest_2)     = parseExpr rest_1
                   eq                   = rest_2 !! 1
               in (Bool (Name eq) expr_1 expr_2, drop 3 rest_2)

parseSet :: [String] -> (Ast, [String])
parseSet (x:xs)
  | isReservedWord x = error ("\nInvalid variable name " ++ show(x) ++ ". That is a reserved word!")
  | otherwise        = let (expr, rest) = parseExpr xs in (Set x expr, rest)


parseVar :: [String] -> (Ast, [String])
parseVar (x:xs) = (Name x, xs)



