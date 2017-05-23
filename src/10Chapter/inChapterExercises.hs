module InChapterExercises where

import Data.Time

-- Exercises: Understanding Folds
-- 1. b foldl (flip (*)) 1 [1..5]
--    c foldl (*) 1 [1..5]

-- 2. foldl (flip (*)) 1 [1..3]
-- (3 (* 2 (1 * 1)))

-- 3. c) foldr, but not foldl, associates to the right

-- 4. a) reduce structure

-- 5. a) foldr (++) "" ["woot", "WOOT", "woot"]
--    b) foldr max "" ["fear", "is", "the", "little", "death"]
--    c) foldr (&&) True [True, False]
--    d) foldr (||) True [False, True]
--    e) foldl (\x y -> show y) "" [1..5]
--    f) foldr (flip const) 'a' [1..5]
--    g) foldr (flip const) 0 "tacos"
--    h) foldl const 0 "burritos"
--    i) foldl const 'z' [1..5]

-- Exercises: Database Processing

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDataBase :: [DatabaseItem]
theDataBase =
  [  DbDate (UTCTime
             (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f [] where
  f (DbDate a) b = a : b
  f _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f [] where
  f (DbNumber a) b = a : b
  f _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr f (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)) where
  f (DbDate a) b = if a > b then a else b
  f _ b = b

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr f 0 where
  f (DbNumber a) b = a + b

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral $ (sumDb xs) `div` (fromIntegral $ length xs) where

fibs    = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- Scans Exercises
-- 1.
fibs20 = take 20 fibs

-- 2.
fibsUnder100 = takeWhile (< 100) fibs
-- filter (\ x -> x < 100) fibs Never ends...

-- 3.
scanFactorial n = take n $ scanl (*) 1 [1..]
