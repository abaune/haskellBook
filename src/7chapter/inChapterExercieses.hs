module InChapterExercises where

myNum :: Num a => a
myNum = 1

myVal :: Num a => a -> a
myVal f = f + myNum

stillAFunction :: [a] -> [a] -> [a] -> [a]
stillAFunction a b c = a ++ b ++ c


addOne :: Integer -> Integer
addOne x = x + 1

bindExp :: Integer -> String
bindExp x = let x = 10; y = 5 in
              "the integer was: " ++ show x
              ++ " and y was: " ++ show y

triple :: Integer -> Integer
triple x = x * 3

-- (\x -> x * 3) :: Integer -> Integer

-- Exercises: Grab Bag
-- 1.) a, b, c, d
-- 2.) a
-- 3.)
-- a.
addOneIfOdd n = case odd n of
  True -> (\n -> n + 1) n
  False -> n
-- b. \x y -> ((if x > y then y else x) + 5)
-- c. \f -> \x -> \y -> f y x

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica Galapagos = False
isSouthAfrica Antarctica = False
isSouthAfrica Australia = False
isSouthAfrica SouthAmerica = False

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  (galapagosPenguin p) || (antarcticPenguin p)

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

-- Exercises: Variety Pack
-- 1. a) The type is (x, y) -> x
--    b) K2 is of type String, it is different from the others
--    c) k1 and k3
-- 2.
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c,f))
f (a, _, c) (d, _, f) = ((a, d),(c, f))

-- Exercieses: Case Practice
-- 1.
functionC x y =
  case x > y of
    True  -> x
    False -> y

-- 2.
ifEvenAdd2 n =
  case isEven of
    True  -> n + 2
    False -> n
  where isEven = even n

-- 3.
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    _  -> 0 -- could also be EQ -> 0


data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _     Coder = LT
codersRuleCEOsDrool e e'        = compare e e'

employeeRank :: (Employee -> Employee -> Ordering)
             -> Employee
             -> Employee
             -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

-- Exercises: Artful Dodgy
-- 1. 1
-- 2. 11
-- 3. 22
-- 4. 21
-- 5. 12
-- 6. 11
-- 7. 21
-- 8. 22
-- 9. 31
-- 10. 23

myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)π
  | otherwise = x

-- Exercises: Guard Duty
-- 1.
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  ='A'
  | y >= 0.8  ='B'
  | y >= 0.7  ='C'
  | y >= 0.59 ='D'
  | otherwise ='F'
  where y = x / 100

-- 2. If C is placed before A, it will get evaluated as True
--    before reaching the A statement, making C always return
--    even when A should return

-- 3. b) true when xs is a palindrome
-- 4. pal can take a list with the Eq instance (Eq x => [x])
-- 5. Eq x => [x] -> Bool
-- 6. c)
-- 7. (Num x, Ord a) => x
-- 8. (Num x, Num y, Ord y) => y -> x
