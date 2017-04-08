module InChapterExercises where

-- Exercises: Mood Swing

data Mood = Blah | Woot deriving Show

-- 1. Mood
-- 2. Blah or Woot
-- 3. changeMood :: Mood -> Woot is wrong because Woot is not a type
-- It should be changeMood :: Mood -> Mood
-- 4. and 5.
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

-- Exercises: Find the Mistakes
-- 1. not True && True
-- 2. not (x == 6)
-- 3. (1 * 2) > 5
-- 4. ["Merry"] > ["Happy"]
-- 5. ['1', '2', '3'] ++ "look at me!"

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool
        then putStrLn "eyyyy. What's shakin'?"
    else
        putStrLn "pshhhh."
    where cool = coolness == "downright frosty yo"


greetIfCool' :: String -> IO ()
greetIfCool' coolness =
    if cool coolness
        then putStrLn "eyyyy. What's shakin'?"
    else
        putStrLn "pshhhh."
    where cool coolness = coolness == "downright frosty yo"
