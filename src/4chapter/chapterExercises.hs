module Chapter4 where

awesome :: [[Char]]
awesome = ["Papuchon", "curry", ":)"]

alsoAwesome :: [String]
alsoAwesome = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, alsoAwesome]

-- 1. length :: [a] -> Int Takes a list and returns an Int
-- 2. a) 5
--    b) 3
--    c) 2
--    d) 5
-- 3. 6 / 3 works
--    6 / length [1,2,3] does not because length returns an Int
--    which is not Fractional
-- 4. 6 `div` length [1,2,3]
-- 5. Type is Bool result is True
-- 6. Type is Bool result is False
-- 7. length allAwesome == 2 (True)
--    length [1, 'a', 3, 'b'] Does not work, list is of different types
--    length allAwesome + length awesome (5)
--    (8 == 8) && ('b' < 'a') (False)
--    (8 == 8) && 9 Does not work, 9 is not a Bool expression
--8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome str = reverse str == str

myAbs :: Integer -> Integer
myAbs x =
    if x < 0
        then x * (-1)
    else
        x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f tup1 tup2 = ((snd tup1, snd tup2), (fst tup1, fst tup2))

-- Correcting syntax
--1.
 -- x = (+)
 -- f xs = w `x` 1
 --   where w = length xs
 -- 2. / X -> x
 -- 3. / (x:xs) -> x
 -- 4. f (a, b) -> a

 -- Match the function names to their types
 -- 1. c) Show a => a -> String
 -- 2. b) Eq a => a -> a -> Bool
 -- 3. a) (a,b) -> a
 -- 4. d) (+) :: Num a => a -> a -> a
