import System.IO
import Data.List
import System.Directory

mainMenu = do
  putStrLn ("\na  create a database")
  putStrLn ("b  delete a database")
  putStrLn ("c  insert an entry to a database")
  putStrLn ("d  print a database")
  putStrLn ("e  select entries from a database")
  putStrLn ("f  delete entries from a database")
  putStrLn ("g  update a database")
  putStrLn ("q  quit\n")
  command <- getLine
  putStrLn("")
  case command of
    "a" -> createDatabase
    "b" -> deleteDatabase
    "q" -> quit
    _   -> mainMenu

-- a  Creating Database
createDatabase = do
  putStrLn ("Enter a database name or type 'b' to go back to the menu")
  db <- getLine
  if null db || db == "b"
    then mainMenu
    else do
      let filename = db ++ ".txt"
      putStrLn ("Enter column names in the form n1,n2,...,n")
      cols <- getLine
      if not $ uniqueColumns (wordsWhen (==',') cols)
        then do
          putStrLn ("Error! Duplicate column labels!")
        else do
          writeFile (db ++ ".txt") (cols ++ "\n")
          putStrLn ("Successfully created database " ++ db)
  -- Go back to menu        
  mainMenu

-- b  Delete database
deleteDatabase = do
  putStrLn ("Enter a database name or type 'b' to go back to the menu")
  db <- getLine
  if null db || db == "b"
    then mainMenu
    else do
      let filename = db ++ ".txt"
      fileDoesExist <- doesFileExist filename
      if fileDoesExist
        then do
          removeFile filename
          putStrLn ("Successfully deleted database " ++ db)
        else putStrLn ("No file with name " ++ db ++ " exists!")
  -- Back to menu
  mainMenu

-- q  Quit program
quit = do
  putStrLn ("Quitter")
  return()


-- Helper Functions

-- Checks if a list of strings are unique
uniqueColumns :: [String] -> Bool
uniqueColumns []     = True
uniqueColumns [x]    = True
uniqueColumns (x:xs)
  | elem x xs        = False
  | otherwise        = uniqueColumns xs

-- Same as words, but can specify the delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


