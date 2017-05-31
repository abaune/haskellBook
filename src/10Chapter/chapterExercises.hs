module ChapterExercises where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

-- 1.
-- a)
tuple3 :: [a] -> [b] -> [(a, b, a)]
tuple3 l1 l2 = [(a, b, c) | a <- l1, b <- l2, c <- l1]

-- b)
onlyP :: [(Char, Char, Char)]
onlyP = filter (\(x,y,z) -> x == 'p') $ tuple3 stops vowels

-- c)
nouns :: [String]
nouns = ["stormtrooper", "jedi", "sith"]

verbs :: [String]
verbs = ["fights", "fires", "forcepushes"]

nounVerbTuples :: [(String, String, String)]
nounVerbTuples = tuple3 nouns verbs

-- 2. This function is finding the average length from a list of words
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- 3.
seekritFunc' x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- Rewriting functions using folds

-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem a list = foldr (\x y -> x == a || y ) False list

-- 4.
myReverse :: [a] -> [a]
myReverse = (foldl . flip) (:) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> (f x) : y) []

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []

-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximum :: (a -> a -> Ordering) -> [a] -> a
myMaximum f xs = foldr (\x y -> if f x y == GT then x else y) (last xs) xs

-- 11.
myMinimum :: (a -> a -> Ordering) -> [a] -> a
myMinimum f xs = foldr (\x y -> if f x y == LT then x else y) (last xs) xs
