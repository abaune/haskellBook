module ChapterExercises where

import Data.Char

-- Multiple Choice

-- 1. a
-- 2. c
-- 3. b
-- 4. c

-- Ciphers

baseValue :: Int
baseValue = ord 'a'

baseString :: String
baseString = "ally"

testString :: String
testString = "meet at dawn"

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

-- As-patterns

isSubSequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubSequenceOf [] [] = True
isSubSequenceOf [] _  = True
isSubSequenceOf _ []  = False
isSubSequenceOf (x:xs) str@(_:ys) =
  if x `elem` str
  then isSubSequenceOf xs ys
  else False

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map f (words str) where
  f str@(x:xs) = (str, toUpper x : xs)


-- Language Exercises

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

-- Note: this feels super dirty. Should probably refactor later on.
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph str = f (words str) [] [] where
  f (x:xs) placeholder acc
    | xs == [] = unwords $ acc ++ (capitalizeWord (last placeholder) : []) ++ (tail (reverse placeholder) ++ [x])
    | (last x == '.' && (length placeholder == 0)) = f xs [] (acc ++ [capitalizeWord x])
    | (last x == '.') = f xs [] (acc ++ (capitalizeWord (last placeholder) : []) ++ (tail (reverse placeholder)) ++ [x])
    | otherwise = f xs (x : placeholder) acc


-- Skipping phone exercies for now

-- Hutton's Razor

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a)   = a
eval (Add a b) = eval a + eval b

printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
