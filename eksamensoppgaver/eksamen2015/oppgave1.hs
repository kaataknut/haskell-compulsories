-- Oppgave 1 - Haskell

-- 1.1)
parse :: String -> Ast
parse = fst . parseExpr

parseExpr :: String -> (Ast, String)
parseExpr (x:xs)
  | x == '+'     = (El expr1 expr2, rest2)
  | x == '*'     = (Og expr1 expr2, rest2)
  | x == '-'     = (Ik expr1, rest1)
  | x == '1'     = (S, xs)
  | x == '0'     = (U, xs)
  | isAlpha x    = (Var x, xs)
  | isSpace x    = parseExpr xs
  | otherwise    = error("Invalid syntax")
  where
    (expr1, rest1) = parseExpr xs
    (expr2, rest2) = parseExpr rest1


-- 1.2)
shorten :: [Char] -> [Char]
shorten [] = []
shorten (x:xs) 
  | elem x xs   = shorten xs
  | otherwise   = x : shorten xs


vars :: Ast -> [Char]
vars = shorten . varsHelp

varsHelp :: Ast -> [Char]
varsHelp S = []
varsHelp U = []
varsHelp (Ik x)  = vars x
varsHelp (Var x) = [x]
varsHelp (Og e1 e2) = vars e1 ++ vars e2
varsHelp (El e1 e2) = vars e1 ++ vars e2


-- 1.3)
tilord :: [Char] -> [[(Char, Ast)]]
tilord [] = [[]]
tilord (x:xs) = let comb = tilord xs
                in [(x, S):s | s <- comb] ++ [(x,U):s | s <- comb]


-- 1.4)
eval :: Ast -> [(Char, Ast)] -> Ast
eval U _           = U
eval S _           = S
eval (Ik a1) ls     = let e1 = eval a1 ls
                          ans = if isTruthy e1 then U else S
                      in ans
eval (El a1 a2) ls = let e1 = eval a1 ls
                         e2 = eval a2 ls
                         truthy = (isTruthy e1 || isTruthy e2)
                         ans = if truthy then S else U
                     in ans
eval (Og a1 a2) ls = let e1 = eval a1 ls
                         e2 = eval a2 ls
                         truthy = (isTruthy e1 && isTruthy e2)
                         ans = if truthy then S else U
                     in ans
eval (Var ch) ls   = let (Just val) = lookup ch ls
                      in val

isTruthy :: Ast -> Bool
isTruthy S = True
isTruthy U = False


-- 1.5)
sat :: String -> [[(Char, Ast)]]
sat [] = [[]]
sat str = satHelper ast comb
  where
    ast = parse str
    vrb = vars ast
    comb = tilord vrb

satHelper :: Ast -> [[(Char, Ast)]] -> [[(Char, Ast)]]
satHelper _ []     = []
satHelper ast (x:xs) 
  | isTruthy (eval ast x)  = x : satHelper ast xs
  | otherwise              = satHelper ast xs

taut :: String -> Bool
taut str = length comb == length (satHelper ast comb)
  where
    ast = parse str
    vrb = vars ast
    comb = tilord vrb