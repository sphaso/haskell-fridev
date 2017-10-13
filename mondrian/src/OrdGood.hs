module OrdGood
    ( someFunc
    ) where

data Color = V | G | R deriving (Eq)

instance Ord Color where
    compare V V = EQ
    compare V _ = GT
    compare G G = EQ
    compare G V = LT
    compare G _ = GT
    compare R R = EQ
    compare R _ = LT


someFunc :: Color -> Color -> Bool
someFunc a b = a > b
