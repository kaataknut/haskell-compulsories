-- Imports
import Data.Char
import Data.Maybe
import Data.List

-- Data types
data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] | Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast | Lambda String Ast
  deriving (Eq, Show, Ord)

-- General Helper functions
isReservedWord :: String -> Bool
isReservedWord str = elem str ["set", "lambda", "case", "otherwise"]

delimiters = "+-*/,;()."

-- Tokenizer
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

-- Parsers
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

-- Memory
type Memory  = (Integer, Integer -> Maybe Ast)

emptyMem :: Memory
emptyMem = (0, \ _ -> Nothing)

lookupMem :: Memory -> Integer -> Maybe Ast
lookupMem (_, func) key = func key

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

addToCtx :: Context -> String -> Integer -> Context
addToCtx (Context ctx) name ptr = Context (\str -> if name == str
                                                   then Just ptr
                                                   else ctx str)

-- Helper functions for Eval funtion
intOps :: [(String, Integer -> Integer -> Integer)]
intOps = [("+", (+)), ("-", (-)), ("*", (*)), ("/", (div))]

eqOps :: [(String, Integer -> Integer -> Bool)]
eqOps = [("==", (==)), ("!=", (/=)), (">", (>)), ("<", (<))]

-- Eval
eval::Ast -> Context -> Memory -> (Ast, Context, Memory)
eval (Block [expr]) ctx mem         = eval expr ctx mem
eval (Block (first:next)) ctx mem   = eval (Block next) ctx2 mem2
  where
    (_, ctx2, mem2) = eval (Block [first]) ctx mem

-- Evaluate Number
eval (Number a) ctx mem             = (Number a, ctx, mem)

-- Evaluate Set patterns
eval (Set str expr) ctx mem         = eval ast ctx3 mem3
  where
    (ast, ctx2, mem2) = eval expr ctx mem
    (ptr1, mem3)    = addToMem mem2 ast   -- Add it to the memory
    ctx3            = addToCtx ctx2 str ptr1 -- Add the variable to the context with the ptr to the address

-- Evaluate a variable
eval (Name str) ctx mem
  | isNothing adr                   = error "Variable is not defined!"
  | otherwise                       = ((fromJust (lookupMem mem (fromJust adr))), ctx, mem)
  where
    adr   = lookupCtx ctx str

-- Evaluate App patterns
eval (App (Name op) [a,b]) ctx mem = let (Number e1, ctx1, mem1) = eval a ctx mem
                                         (Number e2, ctx2, mem2) = eval b ctx1 mem1
                                     in (Number ((fromJust (lookup op intOps)) e1 e2), ctx2, mem2)
-- Evaluate Cases
eval (Case (Bool (Name eq) a b) [exprT, other]) ctx mem
  | caseIsTrue                 = eval exprT ctx2 mem2
  | otherwise                  = eval other ctx2 mem2
  where
    (Number e1, ctx1, mem1) = eval a ctx mem
    (Number e2, ctx2, mem2) = eval b ctx1 mem1
    caseIsTrue              = (fromJust (lookup eq eqOps)) e1 e2

-- Evaluate Default cases
eval (Case Default [expr]) ctx mem  = eval expr ctx mem

run::String -> Ast
run s = let (ast, _, _) = (eval (parse s) emptyCtx emptyMem) in (ast)







