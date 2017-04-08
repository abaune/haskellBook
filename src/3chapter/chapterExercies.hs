module Chapter3Exercies where

-- Reading Syntax

--1. a. Written correctly
-- b. Wrong, fix = (++) [1,2,3] [4,5,6]
-- c. Written correctly
-- d. Wrong, fix = ["hello" ++ "world"]
-- e. Wrong, fix = "hello" !! 4
-- f. Written correctly
-- g. Wrong, fix = take 4 "lovely"
-- h. Written correctly
--
-- 2. a. d
-- b. c
-- c. e
-- d. a
-- e. b

-- Building Functions

-- a.
addExclamation :: String
addExclamation = (++) "Curry is awesome" "!"

--b.
justY :: Char
justY = (!!) "Curry is awesome!" 4

--c.
dropCurry :: String
dropCurry = drop 9 "Curry is awesome!"

--2.
--a.
addEnd :: String -> String
addEnd ch = (++) "Curry is awesome!" ch

--b.
getCharacter :: Int -> Char
getCharacter num = (!!) "Curry is awesome!" num

--c.
dropStuff :: Int -> String
dropStuff num = drop num "Curry is awesome!"

--3.
thirdLetter :: String -> Char
thirdLetter str = (!!) str 3

--4.
letterIndex :: Int -> Char
letterIndex num = (!!) "Curry is awesome!" num

--5.
cia :: String
cia = "Curry is awesome"

rvrs :: String
rvrs = third ++ " " ++ second ++ " " ++ first
  where first  = take 5 cia
        second = ((take 2) . (drop 6)) cia
        third  = drop 9 cia
