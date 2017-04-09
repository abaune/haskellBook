module InChapterExercises where

-- Exercises: Type Matching
-- 1. a) not    c) Bool -> Bool
--    b) length d) [a] -> Int
--    c) concat b) [[a]] -> [a]
--    d) head   a) [a] -> a
--    e) (<)    e) Ord a => a -> a -> Bool

-- Exercises: Type Arguments
-- 1. a) Char -> Char -> Char
-- 2. d) Char
-- 3. d) Num b => b
-- 4. c) Double
-- 5. a) [Char]
-- 6. e) Eq b => b -> [Char]
-- 7. d) (Num a, Ord a) => a
-- 8. a) (Num a, Ord a) => a
-- 9. c) Integer

-- Exercieses: Parametricity
-- 1. Impossible
-- 2. f :: a -> a -> a
--    f a b = a
--    f a b = b
-- 3. f :: a -> b -> b
--    f a b = b

f :: Num a => a -> a -> a
f x y = x + y + 3

f' x y = x + y + 3
