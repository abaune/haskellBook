module ChapterExercises where

import Data.Char

-- 1.
-- :t toUpper
-- toUpper :: Char -> Char

-- t: isUpper
-- isUpper :: Char -> Bool

-- 2. use isUpper
filterUpper :: String -> String
filterUpper = filter (\x -> isUpper x)

-- 3.
capitalize' :: String -> String
capitalize' (x:xs) = toUpper x : xs

-- 4.
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : capitalizeWord xs
capitalizeWord _      = []

-- 5.
firstLetterCapitalized :: String -> Char
firstLetterCapitalized = head . capitalize'

-- 6. already did :)


-- And now the fun begins
-- 1.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x == True = True
  | otherwise = myOr xs

-- 2. Personally, I prefer guard syntax over conditional if/else
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
  if (f x) == True
  then True
  else myAny f xs

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
  | a == x = True
  | otherwise = myElem a xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' a b = any (\x -> a == x) b

-- 4.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = (last xs) : myReverse (init xs)

-- 5.
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f) x ++ squishMap f xs

-- 7.
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\x -> x) xs

-- 8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs) = go f x xs where
  go f a (x:xs)
    | f a x == GT = go f a xs
    | otherwise = go f x xs
  go f x [] = x

-- 9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:[]) = x
myMinimumBy f (x:xs) = go f x xs where
  go f a (x:xs)
    | f a x == LT = go f a xs
    | otherwise = go f x xs
  go f x [] = x

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
