module ChapterExercises where

import Data.List (intersperse)

-- Review of types
-- 1. d) [[Bool]]
-- 2. b) [[3 == 3], [6 > 5], [3 < 4]]
-- 3. d) all of the above
-- 4. b) func "Hello" "World"

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "whoops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. "woops mrow woohoo!"
-- 2. "1 mrow haha"
-- 3. "whoops mrow 2 mrow haha"
-- 4. "pink mrow haha mrow green mrow whoops mrow blue"
-- 5. "are mrow pugs mrow awesome"

-- Recursion
-- 1.
-- otherwise = go (15 - 2) 2 (0 + 1)
-- otherwise = go (13 - 2) 2 (1 + 1)
-- otherwise = go (11 - 2) 2 (2 + 1)
-- otherwise = go (9 - 2) 2 (3 + 1)
-- otherwise = go (7 - 2) 2 (4 + 1)
-- otherwise = go (5 - 2) 2 (5 + 1)
-- otherwise = go (3 - 2) 2 (6 + 1)
-- 1 < 2 = (7, 1)

-- 2.
sumN :: (Eq a, Num a) => a -> a
sumN n = go n 0
  where go n total
          | n == 0    = total
          | otherwise = go (n - 1) (total + n)

-- Works too
-- sumN' 0 = 0
-- sumN' n = n + sumN (n - 1)

-- 3.
multiplyNums :: (Integral a) => a -> a -> a
multiplyNums a b = go a b 0
  where go a b total
          | a == 0    = total
          | otherwise = go (a - 1) b (total + b)

-- Works too
-- multiplyNums 0 _ = 0
-- multiplyNums a b = b + multiplyNums (a - 1) b

-- Fixing dividedBy
data DividedResult =
    Result Integer
  | DividedByZero
  deriving Show

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' a b = go (abs a) (abs b) 0
  where go n d count
         | d == 0 = DividedByZero
         | n < d && a < 0 && b < 0 = Result count
         | n < d && (a < 0 || b < 0) = Result (negate count)
         | n < d && a > 0 && b > 0 = Result count
         | otherwise = go (n - d) d (count + 1)

mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = (mc91 .mc91) (n + 11)

-- Numbers into words

digitToWord :: Int -> String
digitToWord n
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | n == 0 = "zero"
  | otherwise = "not an digit"

digits :: Int -> [Int]
digits n = go n []
  where go val list
         | mod val 10 /= val = go (div val 10) $ (mod val 10) : list
         | otherwise = val : list

wordNumber :: Int -> String
wordNumber n = (concat . intersperse "-" . map digitToWord . digits) n
