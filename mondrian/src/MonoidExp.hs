module MonoidExp where

import Data.Monoid

-- "A monoid is a binary associative operation with an identity"
-- errrrrrr... what?!?


-- identity element
mempty' = mempty == ""

-- operation
mappend' = mappend "a" "b" == "ab"
-- "proof" of identity
identity = "a" <> "" == "a"
-- "proof" of associativity
assoc = ("a" <> "b") <> "c" == "a" <> ("b" <> "c")

-- a little Haskell gift:
-- this just means: foldr mappend mempty
mconcat' = mconcat ["a", "b"] == "ab"

------------------------------------------------------
    --------------------------------------------------

data Somma = Somma Int deriving (Show)

instance Monoid Somma where
    mempty = Somma 0
    mappend (Somma a) (Somma b) = Somma (a + b)

------------------------------------------------------
    --------------------------------------------------

data Capacity = Capacity Int deriving (Show)

instance Monoid Capacity where
    mempty = Capacity 0
    mappend (Capacity a) (Capacity b) = Capacity (mod (a + b) 23)

-- <> and ++ are the same thing!
-- ++ is a specialized instance for lists, but I can still use <>
-- Haskell will pick the right implementation of mappend

listappend = [1, 2, 3] <> [4] == [1, 2, 3] ++ [4]

