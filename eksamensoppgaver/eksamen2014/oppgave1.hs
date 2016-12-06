-- Oppgave 1

-- 1.1
maxsum :: [[Int]] -> Int
maxsum []     = 0
maxsum (x:xs) = maxhelper x + maxsum xs

-- Helper function to retrieve the maximum number of single list
maxhelper :: [Int] -> Int
maxhelper []     = 0
maxhelper [x]    = x
maxhelper (x:xs)
  | x > maxrest    = x
  | otherwise      = maxrest
  where
    maxrest = maxhelper xs

-- 1.2
pack :: Eq(t) => [t] -> [[t]]
pack []  = []
pack xs  = ls:pack rest
  where
    (ls, rest) = packhelper xs

-- Helper function to divide list into two parts
-- (1) The first part consist of the first element + all the next occurences which is eqaul
-- (2) The rest
-- Example: [1, 1, 1, 1, 2, 2, 3, 4, 9] --> ([1, 1, 1, 1], [2, 2, 3, 4, 9])
packhelper :: Eq(t) => [t] -> ([t], [t])
packhelper []     = ([], [])
packhelper [x]    = ([x], [])
packhelper (x:xs)
  | x == head xs  = (x:ls, rest)
  | otherwise     = ([x], xs)
  where
    (ls, rest) = packhelper xs

-- 1.3
lcode :: Eq(t) => [t] -> [(Int, t)]
lcode [] = []
lcode xs = let packed = pack xs
              in map (\x -> (length x, head x)) packed