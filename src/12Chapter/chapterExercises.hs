module ChapterExercies where

-- Determine the kinds
-- 1. id :: a -> a
--    :k of a is *
-- 2. r :: a -> f a
--    :k a is * :k of f a is * -> *

--String processing

-- 1.
notThe :: String -> Maybe String
notThe str
  | str == "the" = Nothing
  | otherwise = Just str

replaceThe :: String -> String
replaceThe str = let xs = fmap notThe $ words str
                     f Nothing = "a"
                     f (Just a) = a
                 in unwords $ fmap f xs

-- 2.
hasVowel :: [String] -> Bool
hasVowel str = ((head . head) str) `elem` "aeiou"

isThe :: String -> Bool
isThe a
  | a == "the" = True
  | otherwise = False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = go (words xs) 0 where
  go (x:xs) count
    | (isThe x) && (hasVowel xs) = go (xs) $ count + 1
    | otherwise = go (xs) count
  go [] count = count

-- 3.

countVowels :: String -> Integer
countVowels str = go str 0 where
  go (x:xs) count
    | x `elem` "aeiou" = go xs $ count + 1
    | otherwise = go xs count
  go [] count = count

-- Validate the word

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

countConst :: String -> Integer -> Integer
countConst [] c = c
countConst (x:xs) c =
  if x `elem` vowels || x == ' '
  then countConst xs c
  else countConst xs $ c + 1

mkWord :: String -> Maybe Word'
mkWord str
    | (countVowels str) > (countConst str 0) = Nothing
    | otherwise = Just $ Word' "Valid"

-- It's only Natural

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger (nat)

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just (getNat x)

getNat :: Integer -> Nat
getNat 0 = Zero
getNat x = Succ $ getNat (x - 1)

-- Small library for Maybe

-- 1.

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just a) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just a) = False

-- 2.

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing = x
mayybee x f (Just y) = f y

-- 3.

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe x (Just y) = y

-- 4.

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x):xs) = x : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs

-- 6.

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = foldr f (Just []) xs where
  f _ Nothing = Nothing
  f Nothing _ = Nothing
  f (Just a) (Just b) = Just $ a : b

-- Small library for Either

-- 1.

lefts' :: [Either a b] -> [a]
lefts' xs = foldr f [] xs where
  f (Left x) xs = x : xs
  f (Right x) xs = xs

-- 2.

rights' :: [Either a b] -> [b]
rights'  = foldr f [] where
  f (Right x) xs = x : xs
  f (Left x) xs = xs

-- 3.

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' = foldr f ([],[]) where
  f (Left x) (xs, ys) = (x:xs, ys)
  f (Right x) (xs, ys) = (xs, x:ys)

-- 4.

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right a) = Just $ f a

-- 5.

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left x) = f x
either' f g (Right x) = g x

-- 6.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Left x) = Nothing
eitherMaybe'' f (Right x) = Just $ either' (\x -> x) f (Right x)

-- Write your own iterate and unfold

-- 1.

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2.

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing -> []
  Just (a, b) -> a : myUnfoldr f b

-- 3.

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

-- Finally something other than a list!

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1.

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (a, b, c) -> Node (unfold f a) b (unfold f c)

-- 2.

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == n then Nothing else Just (x + 1, x, x + 1)) 0
