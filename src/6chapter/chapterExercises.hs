module ChapterExercises where

-- Multiple Choice
--
-- 1. c) makes equality test possible
-- 2. b) is a subclass of Eq
-- 3. a) Ord a => a -> a -> Bool
-- 4. c) the type of x is a Tuple
-- 5. a) Int and Integer numbers
--
-- Does it typecheck?
--
-- 1. it does not compile because Person needs to derive show
--    fix
  data Person = Person Bool deriving (Show)

  printPerson :: Person -> IO ()
  printPerson person = putStrLn (show person)

-- 2. it does not compile because there is no instance of EQ
--    fix
  data Mood = Blah
           | Woot deriving (Show, Eq)

  settleDown x = if x == Woot
                  then Blah
                  else x

-- 3. a) Blah and Woot
--    b) It errors: No instance for (Num Mood) arising from the literal ‘9’
--       this is because Mood does not have an instance of Num
--    c) It errors: No instance for (Ord Mood) arising from a use of ‘>’
--       because there is no instance of Ord

-- 4. Yes the following code typechecks

  type Subject = String
  type Verb = String
  type Object = String

  data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

  s1 = Sentence "dogs" "drool"
  s2 = Sentence "Julie" "loves" "dogs"

-- Given the datatypes, what can we do?

  data Rocks =
    Rocks String deriving (Eq, Show)

  data Yeah =
    Yeah Bool deriving (Eq, Show)

  data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

-- 1. Does not typecheck
--    fix
  phew = Papu (Rocks "chases") (Yeah True)

--  2. typechecks
--  3. typechecks
--  4. Does not typcheck because there is no instance of Ord

-- Match the type

-- 1. Cannot match type
-- 2. Cannot match type
-- 3. Works
-- 4. Works
-- 5. Works
-- 6. Works
-- 7. Cannot match type
-- 8. Cannot match type
-- 9. Works
-- 10.Works
-- 11. Cannot match type

-- Type-Kwon-Do Two: Electric Typealoo

-- 1.
  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk f a b = (f a) == b

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x a = (f a) + (fromInteger x)
