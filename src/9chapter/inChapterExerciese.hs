module InChapterExercises where

import Data.Bool

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

-- Exercise: EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True  = [False, True]
eftBool True False  = []
eftBool True True   = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd EQ LT = []
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT EQ = []
eftOrd GT LT = []
eftOrd GT GT = [GT]

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftInt(succ a) b


eftChar :: Char -> Char -> [Char]
eftChar a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftChar (succ a) b

-- Ecercises: Thy Fearful Symmetry
myWords :: String -> [String]
myWords str
 | str == [] = []
 | head str == ' ' = myWords $ tail str
 | otherwise = (takeWhile (/= ' ') str) : myWords (dropWhile (/= ' ') str)

firstSen :: String
firstSen  = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forest of the night\n"

thirdSen :: String
thirdSen  = "What immortal hand or eye\n"

fourthSen :: String
fourthSen = "Could frame thy fearful symmetry?"

sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines str
  | str == [] = []
  | head str == '\n' = myLines $ tail str
  | otherwise = (takeWhile (/= '\n') str) : myLines (dropWhile (/= '\n') str)

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forest of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

areTheyEqual :: IO ()
areTheyEqual =
  print $ "Are they Equal? "
          ++ show (myLines sentences == shouldEqual)

paramOn :: String -> Char -> [String]
paramOn str param
  | str == [] = []
  | head str == param = paramOn (tail str) param
  | otherwise = (takeWhile (/= param) str) : paramOn (dropWhile (/= param) str) param

-- Exercises: Comprehend Thy Lists
mySqr = [x^2 | x <- [1..5]]

-- Returns squares that are even
getEven = [x | x <- mySqr, rem x 2 == 0]

-- Tuple of squares where first element is less than 50 and second is > 50
-- Will be empty in this case, returns a value if mySqr goes to 10 instead of 5
fiftyTup = [(x,y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

-- Would take the first five from the example above, but will be empty in this case too
fiveFiftyTup = take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

acro xs = [x | x <- xs, elem x ['A'.. 'Z']]

-- Exercieses: Square Cube
myCube = [y^3 | y <- [1..5]]

-- 1.
tupleOutputs = [(x, y) | x <- mySqr, y <- myCube]

-- 2.
tupleOutputsLessThanFifty = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3.
numberOfTuples :: [a] -> Int
numberOfTuples x = length x

-- Exercises: Bottom Madness
-- 1. Bottom
-- 2. returns first element
-- 3. Bottom
-- 4. returns length
-- 5. Bottom
-- 6. returns 2
-- 7. Bottom, no evens
-- 8. returns 1
-- 9. returns 1 3
-- 10. Bottom

-- Intermission: Is it in normal form
-- 1. NF
-- 2. Weak NF
-- 3. NA
-- 4. NA
-- 5. NA
-- 6. NA
-- 7. Weak NF

-- Exercises: More Bottoms
-- 1. Bottom
-- 2. returns 2
-- 3. Bottom
-- 4. Takes a string and returns a list of Booleans if it is a vowel
--    itIsMyster :: String -> [Bool]
-- 5.
--   a. Squares of [1..10]
--   b. [1,10,20]
--   c. [15, 15, 15]
-- 6.
myBool :: [Integer] -> [Integer]
myBool = map (\x -> bool x (-x) $ x == 3)

-- Exercises: Filtering
-- 1.
multiplesOfThree :: [Integer]
multiplesOfThree = filter (\x -> (rem x 3) == 0) [1..30]

-- 2.
multiplesOfThreeLength :: Int
multiplesOfThreeLength = (length . filter (\x -> (rem x 3) == 0)) [1..30]

-- 3.
articles :: [String]
articles = ["the", "a", "an"]

myFilter :: String -> [String]
myFilter xs = (filter (\x -> not $ elem x articles) . words) xs -- the xs isn't necessary here, but I liked to add it to see what was going on

-- Zipping exercises
-- 1.
myZip :: [a] -> [b] -> [(a,b)]
myZip _ []          = []
myZip [] _          = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

-- 2.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ []          = []
myZipWith _ [] _          = []
