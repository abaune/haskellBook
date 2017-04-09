{-# LANGUAGE NoMonomorphismRestriction #-}

module Chapter5 where

-- Multiple Choice
-- 1. c) a list whose elements are all of some type a
-- 2. a) take a list of strings as an argument
-- 3. b) returns one element of type a from a list
-- 4. c) takes a tuple argument and returns the first value
--
-- Determine the type

example = 1

-- 1. a) (* 9) 6 returns 54 :: Num t => t
--    b) head[(0,"doge"),(1,"kitteh")] returns (0,"doge") :: Num t => (t, String)
--    c) head[(0,"doge"),(1,"kitteh")] returns (0,"doge")
--       :: (Integer, [Char])
--    d) if False then True else False returns False :: Bool
--    e) length [1, 2, 3, 4, 5] returns 5 :: Int
--    f) (length [1, 2, 3, 4]) > (length "TACOCAT") returns False :: Bool
--
-- 2. Num a => a
-- 3. Num a => a -> a
-- 4. Fractional a => a :: a
-- 5. [Char]

-- Does it Compile?

-- 1. bigNum = (^) 5 $ 10
--    wahoo = bigNum $ 10
-- breakes because bigNum is not a function
-- fix
bigNum x = (^) 5 $ x
wahoo = bigNum $ 10

-- 2. Compiles and works
x = print
y = print "woohoo!"
z = x "hello world"

-- 3. a = (+)
--    b = 5
--    c = b 10
--    d = c 200
-- breaks because b is a value and cannot apply value to value
-- fix
a = (+)
b = 5
c = a b 10
d = a c 200

-- 4. a = 12 + b
--    b = 10000 * c
-- breaks because c is not defined
--fix
a' = 12 + b'
b' = 10000 * c'
c' = 1

-- Type variable or specific type constructor?

-- 1. fully polymorphic, concrete, concrete
-- 2. fully polymorphic, constrained polymorphic, concrete
-- 3. fully polymorphic, fully polymorphic, concrete

-- Write a type signature
-- 1.
functionH :: [a] -> a
functionH (x:_) = x

-- 2.
functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3.
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function

-- 1.
i :: a -> a
i a = a

-- 2.
c2 :: a -> b -> a
c2 x y = x

-- 3. Yes they are the same

-- 4.
c2' :: a -> b -> b
c2' x y = y

-- 5.
r :: [a] -> [a]
r x = [head x] -- One possibility

-- 6.
co :: (b -> c) -> (a -> b) -> a -> c
co = (.)

-- 7.
a6 :: (a -> c) -> a -> a
a6 f x = x

-- 8.
a6' :: (a -> b) -> a -> b
a6' = ($)

-- Fix it

-- 1.

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"

-- 2.

sing' = if (x < y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"

-- 3.
main :: IO ()
main = do
    print $ 1 + 2
    putStrLn "10"
    print (negate (-1))
    print ((+) 0 blah)
    where blah = negate 1

-- Type-Kwo-Do
--
-- data Woot
-- data Blah
--
-- f :: Woot -> Blah
-- f  = undefined
--
-- g :: (Blah, Woot) -> (Blah, Blah)
-- g (b, w) = (b, f b)

-- 1.

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

-- 2.

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- 3.

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4.

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g = fst . g . f
