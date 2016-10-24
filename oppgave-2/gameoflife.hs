import Data.Array
import Data.Char

data Life = Alive | Dead
newtype Config = Config (Array (Int, Int) Life)


showConfig :: Config -> String
showConfig (Config arr) = show "nothing"

showLife :: Life -> String
showLife Alive = show "Alive"
showLife Dead  = show "Dead"


main = do
  putStrLn ("Type in desired size of gameboard as N M")
  putStrLn ("M represents columns and N represents rows")
  line <- getLine
  if null line
    then return ()
    else do
      let tuple = line
      putStrLn (tuple ++ " yo!")