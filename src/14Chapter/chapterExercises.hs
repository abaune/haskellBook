module ChapterExercises where

  import Test.Hspec
  import Data.List (intersperse)
  import Test.QuickCheck
  import Test.QuickCheck.Gen (oneof)
  import Data.List (sort)

  digitToWord :: Int -> String
  digitToWord n
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | n == 0 = "zero"
    | otherwise = "not an digit"

  digits :: Int -> [Int]
  digits n = go n []
    where go val list
           | mod val 10 /= val = go (div val 10) $ (mod val 10) : list
           | otherwise = val : list

  wordNumber :: Int -> String
  wordNumber n = (concat . intersperse "-" . map digitToWord . digits) n


  main :: IO ()
  main = hspec $ do
    describe "digitToWord" $ do
      it "returns zero for 0" $ do
        digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ do
        digitToWord 1 `shouldBe` "one"

    describe "digits" $ do
      it "returns [1] for 1" $ do
        digits 1 `shouldBe` [1]
      it "returns [1, 0, 0] for 100" $ do
        digits 100 `shouldBe` [1, 0, 0]

    describe "wordNumber" $ do
      it "one-zero-zero given 100" $ do
        wordNumber 100 `shouldBe` "one-zero-zero"
      it "nine-zero-zero-one for 9001" $ do
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"

-- Using QuickCheck

-- 1.

  half :: Double -> Double
  half x = x / 2

  halfIdentity :: Double -> Double
  halfIdentity = (*2) . half

  halfHoldsTrue :: Double -> Bool
  halfHoldsTrue x = halfIdentity x == x

  testHalf :: IO ()
  testHalf = quickCheck halfHoldsTrue

-- 2.

  listOrdered :: (Ord a) => [a] -> Bool
  listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

  listOrderedHoldsTrue :: [Int] -> Bool
  listOrderedHoldsTrue xs
    | listOrdered xs = sort xs == xs
    | otherwise = xs /= sort xs

  testListOrdered :: IO ()
  testListOrdered = quickCheck listOrderedHoldsTrue

-- 3.

  plusAssociative :: Int -> Int -> Int -> Bool
  plusAssociative x y z = x + (y + z) == (x + y) + z

  testPlusAssociate :: IO ()
  testPlusAssociate = quickCheck plusAssociative

  plusCommutative :: Int -> Int -> Bool
  plusCommutative x y = x + y == y + x

  testPlusCommutative :: IO ()
  testPlusCommutative = quickCheck plusCommutative

-- Punting on the rest of the chapter exercises for now. Seems to be self explanatory. Will try to return back later.
