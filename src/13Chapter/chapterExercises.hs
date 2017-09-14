module ChapterExercises where

  import Control.Monad
  import System.Exit (exitSuccess)
  import Data.Char

-- 1.
  baseValue :: Int
  baseValue = ord 'a'

  runCeaser :: IO ()
  runCeaser = do
    putStr "Enter your string: "
    list <- getLine
    putStr "Enter shiftSize: "
    shiftSize <- getLine
    putStrLn $ "Ceaser encryption : " ++ ceaser list (read shiftSize)

  runVigenere :: IO ()
  runVigenere = do
    putStr "Enter first string: "
    s1 <- getLine
    putStr "Enter second string: "
    s2 <- getLine
    putStrLn $ "Vigenere encryption: " ++ vigenere s1 s1 0

  ceaser :: String -> Int -> String
  ceaser (x:xs) shiftSize = (chr
                          . (+baseValue)
                          . (`mod` 26)
                          . (`mod` baseValue)
                          . (+shiftSize)
                          . ord) x : ceaser xs shiftSize
  ceaser [] _ = []

  vigenere :: String -> String -> Int -> String
  vigenere bs (t:ts) i
    | t == ' ' = ' ' : vigenere bs ts i
    | i == (length bs - 1) = (encode (bs !! i) t) : vigenere bs ts 0
    | i < (length bs) = (encode (bs !! i) t) : (vigenere bs ts $ i + 1)

  vigenere bs [] _ = []

  encode :: Char -> Char -> Char
  encode encodeStr str = chr shiftVal where
                          ordSum = (ord str) + (ord encodeStr)
                          shiftVal = ((+baseValue)
                                    . (`mod` 26)
                                    . (`mod` baseValue)) ordSum

-- 2.
  palindrome :: IO ()
  palindrome = forever $ do
    line1 <- getLine
    case (line1 == reverse line1) of
      True -> putStrLn "It's a palindrome!"
      False -> putStrLn "Nope!"
    exitSuccess

-- 3.

  palindrome' :: IO ()
  palindrome' = forever $ do
    putStr "Enter your text: "
    line1 <- getLine
    let newList = removeNonAlpha $ convertToLower line1
    case (newList == reverse newList) of
      True -> putStrLn "It's a palindrome!"
      False -> putStrLn "Nope!"
    exitSuccess

  convertToLower :: String -> String
  convertToLower list = map toLower list

  removeNonAlpha :: String -> String
  removeNonAlpha list = foldr (\x y -> if isAlpha x then x : y else y) [] list

-- 4.
  type Name = String
  type Age = Integer

  data Person = Person Name Age deriving Show

  data PersonInvalid = NameEmpty
                     | AgeTooLow
                     | PersonInvalidUnknown String
                     deriving (Eq, Show)

  mkPerson :: Name
           -> Age
           -> Either PersonInvalid Person
  mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
                         "Name was: " ++ show name ++
                         " Age was: " ++ show age

  gimmePerson :: IO ()
  gimmePerson = do
    putStr "Enter name: "
    name <- getLine
    putStr "Enter age: "
    age <- getLine
    let person = mkPerson name (read age)
    case (person) of
      Right person -> putStrLn $ "Yay! Successfully got a person: " ++ show person
      Left _ -> putStrLn $ "Error: " ++ show person
