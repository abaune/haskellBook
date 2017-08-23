{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module InChapterExercises where

import Data.Int

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

-- Exercises: Dog Types

-- 1. type constructor
-- 2. * -> *
-- 3. *
-- 4. Num a => Doggies a
-- 5. Husky (10 :: Integer) :: Doggies Integer
-- 6. Mastiff "Scooby Doo" :: Doggies [Char]
-- 7. both type and data constructor
-- 8. DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- 9. DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]

data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving(Eq, Show)

-- Exercises: Vehicles

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size 666)

-- 1. myCar :: Vehicle
-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _ ) = True
isCar _          = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _ ) = True
isPlane _            = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3.

getManu :: Vehicle -> Manufacturer
getManu (Car manu _ ) = manu

-- 4. Using on plane data will give a non-exhaustive patterns error
-- 5. Done above tested with getSize
getSize :: Vehicle -> Size
getSize (Plane _ s) = s

data Example0 =
  Example0 deriving (Eq, Show)

data Example1 =
  Example1 Int deriving (Eq, Show)

data Example2 =
  Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)

-- Exercises: Cardinality

-- 1. 1
-- 2. 3
-- 3. 65535
-- 4. Int is bound, Integer is not
-- 5. 2^8

data Example = MakeExample deriving Show

-- Exercises: For Example
-- 1. MakeExample :: Example. We get an error when requesting type of
--    Data constructor not in scope : Example

-- 2. data Example = MakeExample
--    instance [safe] Show Example

-- 3.
data SingleExample = MakeSingleExample Int deriving Show
-- MakeSingleExample :: Int -> SingleExample (Needs an Int to return a SingleExample)

-- newtype Goats =
--   Goats Int deriving (Eq, Show)

newtype Cows =
  Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Show, Eq, TooMany)

-- Exercises: Logic Goats

-- 1.

instance TooMany (Int, String) where
  tooMany (i, _) = i > 42

-- 2.

instance TooMany (Int, Int) where
  tooMany (a, b) = a + b > 42

-- 3.

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, a') = tooMany (a + a')

-- Exercises: Pity the Bool

-- 1.
data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- The cardinality is 4. Bool a cardinality of 2, two times

-- 2.
data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- myNumba = Numba (-128)

-- Cardinality is 258. 256 for Int8 + 2 for Bool.
-- Breaks on numbers not in scope of Int8

data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth deriving (Eq, Show)

data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

-- data Person = MkPerson String Int deriving (Eq, Show)
--
-- jm = MkPerson "julie" 108
-- ca = MkPerson "chris" 16
--
-- namae :: Person -> String
-- namae (MkPerson s _) = s

data Person =
  Person {name :: String
         , age :: Int}
         deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16

-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show
--
-- data BookType = FictionBook FictionBook
--               | NonfictionBook Nonfiction
--               deriving Show

type AuthorName = String

data Author =
    Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

data Expr =
    Number Int
  | Add Expr Expr
  | Minue Expr
  | Mult Expr Expr
  | Divide Expr Expr

-- Exercises: How Does Your Garden Grow?

-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show

type Gardener = String

-- data Garden =
--   Garden Gardener FlowerType deriving Show

-- The normal form is:

data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show

data GuessWhat =
  ChickenButt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
    First  a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst  :: a
                , psecond :: b }
                deriving (Eq, Show)

newtype NumCow =
  Numcow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmHouse =
  BigFarmHouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmHouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = ChickenButt

idInt :: Id Integer
idInt = MkId 10

idIdentity :: Id (a -> a)
idIdentity = MkId (\x -> x)

-- type Awesome = Bool
-- type Name = String
--
-- person :: Product Name Awesome
-- person = Product "Simon" True

data Twitter =
  Twitter deriving (Eq, Show)

data AskFm =
  AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

-- data SocialNetwork =
--     Twitter
--   | AskFm
--   deriving (Eq, Show)
--
-- type Twitter = String
-- type AskFm = String
--
-- twitter :: Sum Twitter AskFm
-- twitter = First "Twitter"
--
-- askfm :: Sum Twitter AskFm
-- askfm = First "AskFm"

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 42
                         , psecond = 0.00001 }

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
             deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }

-- Exercise: Programmers

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer { os = opSys, lang = language}
     | opSys    <- allOperatingSystems
     , language <- allLanguages
  ]

newtype FarmerName = FarmerName String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer =
  Farmer FarmerName Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
  FarmerRec { name'       :: FarmerName
            , acres       :: Acres
            , farmerType  :: FarmerType } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
  DairyFarmer -> True
  _           -> False

-- Exercises: The Quad

-- 1. 8
-- 2. 16
-- 3. 256
-- 4. 8
-- 5. 16
-- 6. 65536

data Silly a b c d = MkSilly a b c d deriving Show

-- Binary TakeYourChancesUnited

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left a right) = [a] ++ preOrder left ++ preOrder right

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left a right) = inOrder left ++ [a] ++ inOrder right

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left a right) =  postOrder left ++  postOrder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preOrder testTree == [2, 1, 3]
  then putStrLn "PreOrder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inOrder testTree == [1, 2, 3]
  then putStrLn "inOrder fine!"
  else putStrLn "Bad news bears."

testPostOrder :: IO ()
testPostOrder =
  if postOrder testTree == [1, 3, 2]
  then putStrLn "postOrder fine!"
  else putStrLn "Bad news bears."

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) = foldr f b (preOrder (Node left a right))
