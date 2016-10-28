import Data.Array
import Data.Char
import Data.List

data Life = Alive | Dead
  deriving (Read)
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

groupInto :: Int -> [a] -> [[a]]
groupInto _ [] = []
groupInto n xs
  | n > 0      = (take n xs) : (groupInto n (drop n xs))
  | otherwise  = error "Cannot group into zero or negative!"

lifeToString :: [Life] -> [Char]
lifeToString [] = []
lifeToString (Alive:xs) = 'O':lifeToString xs
lifeToString (Dead:xs)  = '-':lifeToString xs