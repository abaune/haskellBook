module InChapterExercises where

data Trivial =
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Ord, Show)

-- day of week and numerical day of month
data Date =
    Date DayOfWeek Int deriving (Show)

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _     = False

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'

data Identity a =
    Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

-- Exercises : Eq Instances

-- 1.
data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn b) = a == b

-- 2.
data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b)  (Two x y) = a == x && b == y

-- 3.
data StringOrInt =
      TisAnInt   Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b)     = a == b
    (==) (TisAString a) (TisAString b) = a == b
    (==) _ _                           = False

-- 4.
data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair x y) = a == x && b == y

-- 5.
data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple x y) = a == x && b == y

-- 6.
data Which a =
      ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne b) = a == b
    (==) (ThatOne a) (ThatOne b) = a == b
    (==) _ _                     = False

-- 7.
data EitherOr a b =
      Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello b)     = a == b
    (==) (Goodbye a) (Goodbye b) = a == b
    (==) _ _                     = False

-- Exercises: Tuple Experiment
-- divMod :: a -> a -> (a, a)
-- it does both div and mod
--
-- quotRem :: a -> a -> (a, a)
-- it does both quot and rem

divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

check' :: Ord a => a -> a -> Bool
check' a a' = a == a'

-- Exercieses: Will They Work?
-- 1. max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
--    works and returns 5
--
-- 2. compare(3*4)(3*5)
--    works and returns LT
--
-- 3. compare "Julie" True
--    does not work because String and bool are different types
--
-- 4. (5+3)>(3+6)
--    works and returns False
