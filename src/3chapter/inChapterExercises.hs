module Chapter3 where

main :: IO ()
main = putStrLn "hello world!"

main' :: IO ()
main' = do
    putStrLn "Count to four for me:"
    putStr   "one, two"
    putStr   ", three, and"
    putStrLn " four!"

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main''' :: IO ()
main''' = do
    putStrLn myGreeting
    putStrLn secondGreeting
      where secondGreeting = concat [hello, " ", world]

-- TopOrLocal

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
  where woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5

-- Exercises: Scope
-- 1. Yes
-- 2. No
-- 3. No
-- 4. Yes

-- Exercises: Syntax Errors
-- 1. Will not compile, fix = (++) [1,2,3] [4,5,6]
-- 2. Will not compile, fix = "<3" ++ "Haskell"
-- 3. Will compile concat ["<3", " Haskell"]

-- print3flipped

myGreeting' :: String
myGreeting' = (++) "hello" "world"

hello' :: String
hello' = "hello"

world' :: String
world' = "world!"

main'' :: IO ()
main'' = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting =
            (++) hello' ((++) " " world')
