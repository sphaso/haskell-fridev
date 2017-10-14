module FunctorExp where

-- A functor is a way to apply a function over or around some structure that we don’t want to alter

listmap = map (+1) [1, 2, 3] == fmap (+1) [1, 2, 3]

-- structure = Maybe Int
-- not alterling = return structure is the same as input structure
addOneMaybe :: Maybe Int -> Maybe Int
addOneMaybe boh = fmap (+1) boh

-- by structure we only mean the "f" in "f a"
-- in this case "Maybe"
-- no promises are made on the value :)
convert :: Maybe String -> Maybe Int
convert boh = fmap read boh
