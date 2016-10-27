import Data.Array
import Data.Char

data Life = Alive | Dead
instance Show Life where
  show c = showLife c

newtype Config = Config (Array (Int, Int) Life)
instance Show Config where
  show c = showConfig c


showConfig :: Config -> String
showConfig (Config arr) = show arr

showLife :: Life -> String
showLife Alive = show "Alive"
showLife Dead  = show "Dead"


main = do
  putStrLn ("Type in desired size of gameboard as N M")
  putStrLn ("N represents columns and M represents rows")
  line <- getLine
  if null line
    then return ()
    else do
      let tuple = tuplify $ words line
      let cols  = fst tuple
      let rows  = snd tuple
      let arr   = Config (array ((1,1), (cols, rows)) [((i, j), Dead) | i <- [1..cols], j <- [1..rows]])
      putStrLn (show(arr))



tuplify :: [String] -> (Int, Int)
tuplify []               = error "Cannot tuplify empty string"
tuplify [x]
  | isNumb x             = (read x, read x)
  | otherwise            = error (show(x) ++ " is not a number!")
tuplify (x:y:xs)
  | isNumb x && isNumb y = (read x, read y)
  | otherwise            = error (show(x) ++ " and " ++ show(y) ++ " must both be numbers!")

isNumb :: String -> Bool
isNumb []  = False
isNumb xs  = all isDigit xs