module ChapterExercises where

-- 1. d) may resolve to values of different types, depending on inputs
-- 2. b) Char -> [String]
-- 3. d) (Ord a, Num a) => a -> Bool
-- 4. b) is a higher order function
-- 5. a) f True :: Bool

-- Let's write code
-- 1.
tenDigit :: Integral a => a -> a
tenDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- a.)
tenDigit' :: Integral a => a -> a
tenDigit' x = d
  where xLast = fst $ divMod x 10
        d     = snd $ divMod xLast 10

-- b.) Yes same type
-- c.)
hunsD :: Integral a => a -> a
hunsD x = d
  where xLast = fst $ divMod x 100
        d     = snd $ divMod xLast 10

-- 2.
foldBool :: a -> a -> Bool -> a
foldBool a b c =
  case c of
    True  -> a
    False -> b

foldBool' :: a -> a -> Bool -> a
foldBool' a b c
  | c == True = a
  | otherwise = b

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, b) = (f a, b)

-- 4. load code in separate file and play with it!
-- 5.
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

-- 6.
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read .show

mainRoundTrip = do
  print ((roundTrip' 4) :: Int)
  print $ id 4
