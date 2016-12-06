-- Oppgave 2

-- 2.1
tails :: [a] -> [[a]]
tails [] = [[]]
tails ls = ls : tails (tail ls)

-- 2.2
com :: Int -> [t] -> [[t]]
com 0 xs = [[]]
com y xs = [[z] ++ p | z <- xs, p <- (com (y-1) xs)]