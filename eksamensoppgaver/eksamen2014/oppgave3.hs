-- Task 3

{-
-- 3.1
Språk L
Alfabet = {a, b, c}
Startsymbol S

Bokstav = a | b | c
S = abG | Bokstav S
G = abF | Bokstav G
F = Bokstav F | ''


-- 3.2
Parse tree for "aabab"

               a   
             /   \
            /     \
           S      Bokstav

???


-}

-- 3.3
aksept :: String -> Bool
aksept []   = False
aksept (xs) = found1 && found2
  where
    (found1, list) = aksepthelper xs
    (found2, rest) = aksepthelper list

aksepthelper :: String -> (Bool, String)
aksepthelper []          = (False, [])
aksepthelper [x]         = (False, [x])
aksepthelper (x:y:xs)
  | x == 'a' && y == 'b' = (True, xs)
  | otherwise            = (result, rest)
  where
    (result, rest) = aksepthelper (y:xs)

-- 3.4
trans :: String -> String
trans str = transhelper str 0

transhelper :: String -> Int -> String
transhelper [] n           = if n > 1 then "" else "FEIL!"
transhelper ('a':'b':xs) n = 'd':transhelper xs (n+1)
transhelper (x:xs) n
  | x == 'a' || x == 'b'
             || x == 'c'   = x : transhelper xs n
  | otherwise              = "FEIL"