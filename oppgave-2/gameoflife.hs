import Data.Array
import Data.Char
import Data.List

data Life = Alive | Dead
  deriving (Read, Eq)
instance Show Life where
  show c = showLife c

newtype Config = Config (Array (Int, Int) Life)
instance Show Config where
  show c = showConfig c

showConfig :: Config -> String
showConfig (Config arr) = unlines grouped 
  where
    (_, (n,m)) = bounds arr
    xs         = lifeToString (elems arr)
    grouped    = groupInto (n+1) xs

showLife :: Life -> String
showLife Alive = show('O')
showLife Dead  = show('-')

-- array ((0,0), (4,4)) [((i, j), Dead) | i <- [0..4], j <- [0..4]]
-- Config (array ((0,0), (n,m)) [((i, j), Dead) | i <- [0..n], j <- [0..m]])
-- [((1,0),Alive),((2,1),Alive),((0,2),Alive),((1,2),Alive),((2,2),Alive)]
main = do
  putStrLn ("Type in desired size of gameboard as (N,M)")
  putStrLn ("N represents columns and M represents rows")
  line <- getLine
  if null line
    then return ()
    else do
      let bounds = read line::(Int,Int)
      let m  = -1 + fst bounds
      let n  = -1 + snd bounds
      let arr = array ((0,0), (n,m)) [((i, j), Dead) | i <- [0..n], j <- [0..m]]
      putStrLn ("Type in the starting point: ")
      line <- getLine
      let list = read line::[((Int,Int),Life)]
      let updatedArr = arr // list
      let board = Config (updatedArr)
      putStrLn(show(board))
      checkNextState board


checkNextState :: Config -> IO ()
checkNextState c = do
                    putStrLn("Press ’q’ to quit or ’Enter’ to generate a new configuration")
                    command <- getChar
                    if (command /= 'q')
                      then do
                        let nextState = nextGeneration c
                        print nextState
                        checkNextState nextState
                        else return ()

nextGeneration :: Config -> Config
nextGeneration (Config c) = board
  where
    (_, (n, m))    = bounds c
    arr            = array ((0,0), (n,m)) [((i, j), Dead) | i <- [0..n], j <- [0..m]]
    arr2           = arr // format (livecells (Config c))
    board          = Config arr2

groupInto :: Int -> [a] -> [[a]]
groupInto _ [] = []
groupInto n xs
  | n > 0      = (take n xs) : (groupInto n (drop n xs))
  | otherwise  = error "Cannot group into zero or negative!"

lifeToString :: [Life] -> [Char]
lifeToString [] = []
lifeToString (Alive:xs) = 'O':lifeToString xs
lifeToString (Dead:xs)  = '-':lifeToString xs

-- (1,1), (x,y)
-- (0,0),      (1,0),  (2,0),     (2,1),    (2,2),     (1,2),    (0,2),     (0,1)
-- (x-1,y-1), (x,y-1), (x+1,y-1), (x+1, y), (x+1, y+1) (x, y+1)  (x-1, y+1) (x-1, y)
neighbs :: Config -> (Int,Int) -> [(Int,Int)]
neighbs c (x,y) = map (wrap c) [(x-1, y-1),(x, y-1),
                                (x+1, y-1),(x+1,y),
                                (x+1,y+1),(x,y+1),
                                (x-1,y+1),(x-1,y)]

wrap ::  Config -> (Int, Int) -> (Int, Int)
wrap (Config c) (x,y)  = ((x `mod` (n +1)), (y `mod` (m+1))) 
  where
    (_,(n,m)) = bounds c

liveneighbs :: Config -> (Int, Int) -> Int
liveneighbs c p = length (filter (isAlive c) (neighbs c p))

isAlive :: Config -> (Int,Int) -> Bool
isAlive (Config c) p = c!p == Alive

survivors :: Config -> [(Int,Int)]
survivors (Config c) = [p | p <- pos, (isAlive (Config c) p) && elem (liveneighbs (Config c) p) [2,3]]
  where
    pos = indices c

births :: Config -> [(Int,Int)]
births (Config c) = [p | p <- pos, not (isAlive (Config c) p) && elem (liveneighbs (Config c) p) [3]]
  where
    pos = indices c

livecells :: Config -> [(Int,Int)]
livecells c = births c ++ survivors c

format :: [(Int,Int)] -> [((Int,Int), Life)]
format xs = [(p, Alive) | p <- xs] 