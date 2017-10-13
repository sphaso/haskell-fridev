module Ord
    ( someFunc
    ) where

data Color = V | G | R

someFunc :: Color -> Color -> Bool
someFunc a b = a > b
