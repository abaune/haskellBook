module Learn where

sayHello :: String -> IO ()
sayHello x = putStrLn ("hello, " ++ x ++ "!")

triple :: Int -> Int
triple x = x * 3

-- Exercises: Comprehension Check
--
-- 1. let half x = x / 2
--    let square x = x * x
--
-- 2. doublePie x = 3.14 * (x * x)
--
-- 3. doublePie x = pi * (x * x)

-- Exercises: Parentheses and Association
-- 1. a) 8 + 7 * 9 = 71 (*) higher precedence
--    b) (8 + 7) * 9 = 135 () higher precedence
--
-- 2. a) perimeter x y = (x * 2) + (y * 2)
--    b) perimeter x y = x * 2 + y * 2
--       Will have the same result because * higher precedence than +
--
-- 3. a) f x = x / 2 + 9
--    b) f x = x / (2 + 9)
--       Will evaluate () first

x = 10 * 5 + y

myResult :: Int
myResult = x * 5

y = 10

foo :: Int -> Int
foo x =
    let y = x * 2
        z = x ^ 2
    in 2 * y * z


-- Exercises: Healing the Sick
--
-- 1. let area x = 3. 14 * (x * x)
--    let area x = 3.14 * (x * x)
--
-- 2. let double x = b * 2
--    let double x = x * 2
--
-- 3. x = 7
--     y = 10
--    f = x + y
--
--    x = 7
--    y = 10
--    f = x + y

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

-- Exercieses: A Head Code
--
-- 1. Will return 5
-- 2. Will return 25
-- 3. Will return 30
-- 4. Will return 6

mult1     = x * y
  where x = 5
        y = 6

let1      = x * 3 + y
  where x = 3
        y = 1000

let2      = x * 5
  where y = 10
        x = 10 * 5 + y

let3      = z / x + y
  where x = 7
        y = negate x
        z = y * 10    
