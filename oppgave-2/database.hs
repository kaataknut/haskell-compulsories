import System.IO
import Data.List
import System.Directory
import Data.Char
import Data.Maybe

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
    "c" -> insertEntry
    "d" -> printDatabase
    "e" -> selectFromDatabase
    "f" -> deleteFromDatabase
    "q" -> quit
    _   -> mainMenu

-- a  Creating Database
createDatabase = do
  db <- getDBName
  let goToMenu = goBackToMenu db

  if goToMenu then return ()
  else do
    let filename = db ++ ".txt"
    fileDoesExist <- doesFileExist filename

    if fileDoesExist then putStrLn ("Already exists database with name " ++ db ++ "!")
    else do
      putStrLn ("Enter column names in the form n1,n2,...,n")
      cols <- getLine
      if      elem ' '                                cols then putStrLn("Illegal with spaces in column labels!")
      else if not $ uniqueColumns (wordsWhen (==',') cols) then putStrLn("Illegal with duplicate column labels!")
      else do
        writeFile (db ++ ".txt") (cols ++ "\n")
        putStrLn ("Successfully created database " ++ db)
  mainMenu

-- b  Delete database
deleteDatabase = do
  db <- getDBName
  let goToMenu = goBackToMenu db
  
  if goToMenu then mainMenu
  else do
    let filename = db ++ ".txt"
    fileDoesExist <- doesFileExist filename
    if not fileDoesExist then putStrLn("No database with name " ++ db ++ " exists!")
    else do
      removeFile filename
      putStrLn ("Successfully deleted database " ++ db)
  mainMenu

-- c  Inserting row
insertEntry = do
  db <- getDBName
  let goToMenu = goBackToMenu db

  if goToMenu then return ()
  else do
    let filename = db ++ ".txt"
    fileDoesExist <- doesFileExist filename
    if not fileDoesExist then putStrLn("No database with name " ++ db ++ " exists!")
      else do
      handle <- openFile filename ReadMode
      contents <- hGetContents handle
      let (line1:_)   = lines contents
      let columnCount = length (wordsWhen (==',') line1)
      
      putStrLn ("Enter fields in the form n1,n2,...,n")
      fields <- getLine
      let fieldCount = length (wordsWhen (==',') fields)
      
      if columnCount == fieldCount
      then do 
        hClose handle
        appendFile filename (fields ++ "\n")
      else do
        hClose handle
        putStrLn ("You did not supply the right amount of fields!")
      -- Back to menu
  mainMenu

-- d  Print database
printDatabase = do
  db <- getDBName
  let goToMenu = goBackToMenu db
  if goToMenu then return ()
  else do
    let filename = db ++ ".txt"
    fileDoesExist <- doesFileExist filename
    if not fileDoesExist then putStrLn("No database with name " ++ db ++ " exists!")
    else do
      handle <- openFile filename ReadMode
      contents <- hGetContents handle
      putStrLn("\n" ++ contents)
  mainMenu

-- e  Select from Database
selectFromDatabase = do
  db <- getDBName
  let goToMenu = goBackToMenu db
  
  if goToMenu then return ()
  else do
    let filename = db ++ ".txt"
    fileDoesExist <- doesFileExist filename
    if not fileDoesExist then putStrLn("No database with name " ++ db ++ " exists!")
    else do
      -- Get all info
      putStrLn ("Enter a column label")
      col <- getLine
      operator <- getOperator
      putStrLn("Enter a value to compare")
      rawVal <- getLine
      let value = rawVal

      -- Open File and get contens
      handle <- openFile filename ReadMode
      contents <- hGetContents handle
      let (line1:rest)   = lines contents
      let columns = wordsWhen (==',') line1

      -- Typed in value decides string/int
      let datatype = checkType value

      if datatype == 'e' then return ()
      else do
        let fieldNo =  elemIndex col columns
        if isNothing fieldNo then putStrLn("No column with name " ++ col ++ " in database " ++ db ++ "!")
        else do
          let index = fromJust fieldNo
          let rows = toRowsList rest
          let sel = filter (filterHelper datatype operator value index) rows
          putStrLn ("\n" ++ line1)
          putStrLn (replicate (length line1) '-')
          putStrLn (formatList sel)
  mainMenu

-- f  Delete from Database
deleteFromDatabase = do
  db <- getDBName
  let goToMenu = goBackToMenu db

  if goToMenu then return()
  else do
    let filename = db ++ ".txt"
    fileDoesExist <- doesFileExist filename
    if not fileDoesExist then putStrLn("No database with name " ++ db ++ " exists!")
    else do
      putStrLn ("Enter a column label")
      col <- getLine
      operator <- getOperator
      putStrLn("Enter a value to compare")
      rawVal <- getLine

      let datatype = checkType rawVal

      if datatype == 'e' then return ()
        else do
        -- Open File and get contens
        contents <- readFile filename
        let (line1:rest)   = lines contents
        let columns = wordsWhen (==',') line1

        let fieldNo =  elemIndex col columns
        if isNothing fieldNo then do
         putStrLn("No column with name " ++ col ++ " in database " ++ db ++ "!")
        else do
          let index = fromJust fieldNo
          let rows = toRowsList rest
          let leftovers = filter (not . filterHelper datatype operator rawVal index) rows
          let filecontent = line1 ++ "\n" ++ (formatList leftovers)
          length contents `seq` (writeFile filename filecontent)
  mainMenu


-- q  Quit program
quit = do
  putStrLn ("Quitter")
  return()



filterHelper :: Char -> String -> String -> Int -> [String] -> Bool
filterHelper t op v i xs
  | t == 'i'   = selectIntFunc op (read(xs!!i)) (read v)
  | t == 's'   = selectStrFunc op (xs!!i) v


formatList :: [[String]] -> String
formatList [] = ""
formatList xs = unlines $ map unwords xs

toRowsList :: [String] -> [[String]]
toRowsList []     = []
toRowsList (x:xs) = (wordsWhen (==',') x):toRowsList xs 

getOperator :: IO String
getOperator = do
                putStrLn("Enter ’<’,’>’,’>=’,’<=’,’==’,’/=’ without the quotations")
                op <- getLine
                if elem op ["<",">",">=","<=","==","/="]
                  then return (op)
                else getOperator

getDBName :: IO String
getDBName = do
              putStrLn ("Enter a database name or type 'b' to go back to the menu")
              db <- getLine
              return (db)

goBackToMenu :: String -> Bool
goBackToMenu [] = True
goBackToMenu s  = s == "b"

checkType :: String -> Char
checkType s 
  | all isDigit s       = 'i'
  | not (all isDigit s) = 's'
  | otherwise           = 'e'

selectIntFunc :: String -> (Int -> Int -> Bool)
selectIntFunc l 
  | l == "<" = (<)
  | l == ">" = (>)
  | l == ">=" = (>=)
  | l == "<=" = (<=)
  | l == "==" = (==)
  | l == "/=" = (/=)

selectStrFunc :: String -> (String -> String -> Bool)
selectStrFunc l 
  | l == "<" = (<)
  | l == ">" = (>)
  | l == ">=" = (>=)
  | l == "<=" = (<=)
  | l == "==" = (==)
  | l == "/=" = (/=)

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