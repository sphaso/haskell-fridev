module Ord
    ( someFunc
    ) where

-- There is a hierarchy of colors: V is better than G which is better than R

-- Do you think this will run?
-- In dynamic or non-compiled languages you'll be lucky to get a warning!

data Color = V | G | R

someFunc :: Color -> Color -> Bool
someFunc a b = a > b

-- But that's not the point. We might as well use Java!
