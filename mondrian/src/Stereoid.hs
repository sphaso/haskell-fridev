module Stereoid where

import Data.Monoid ((<>))

data Color = V | G | R deriving (Eq, Show)

instance Ord Color where
    compare V V = EQ
    compare V _ = GT
    compare G G = EQ
    compare G V = LT
    compare G _ = GT
    compare R R = EQ
    compare R _ = LT

data QuoteData = QuoteData { province :: String, isBersani :: Bool, isNewInsurance :: Bool, chosenClaimsExperience5 :: Int, chosenClaimsExperience2 :: Int, chosenClaimsExperienceYear :: Int }

-- We're looking for something not yet formalized
-- A particular algebraic structure with two binary operations
-- both associative and commutative among themselves

-- For lack of better terms... I'll call them "stereoids"!
-- They're like two monoids, one unraveling the other

-- Since no one thought about them before
-- we'll have to make our own typeclass

class Stereoids s where
    identity :: s
    invIdentity :: s
    add :: s -> s -> s
    subt :: s -> s -> s

-- for our purposes, add = Restrictive, subtract = Widening

instance Stereoids Color where
    identity = V
    invIdentity = R
    add = min
    subt = max

restrict quote = foldr add identity (fmap ($ quote) [isRedProvince, bersani])
widen quote = foldr subt invIdentity (fmap ($ quote) [chosenClaimsExperience, bersani])

calculate quote = subt (widen quote) (restrict quote)

-----------------------------------------------------------------------------------
    -------------------------------------------------------------------------------
        ---------------------------------------------------------------------------

bersani :: QuoteData -> Color
bersani QuoteData{isBersani=True} = G
bersani _ = V

redProvince :: [String]
redProvince = ["NA", "CE", "SA", "BA", "BT", "FG", "TA", "VV", "CZ", "LT", "PO", "AV", "BR", "KR", "RC"]

isRedProvince :: QuoteData -> Color
isRedProvince QuoteData{province=prov} =  case prov `elem` redProvince of
                                            True -> V
                                            _    -> R

chosenClaimsExperience :: QuoteData -> Color
chosenClaimsExperience QuoteData{isNewInsurance=True} = V
chosenClaimsExperience QuoteData{chosenClaimsExperience5=five, chosenClaimsExperience2=two, chosenClaimsExperienceYear=year}
  | stringa == "0-0" = V
  | stringa == "5Y-0" = V
  | stringa == "4Y-0" = V
  | stringa `elem` ["3Y-0", "2Y-1", "1Y-1", "2-0", "2-1", "2-2", "2->0", ">2->=0"] = R
  | otherwise = G
  where stringa = (chosenClaimsFive year five) <> (chosenClaimsTwo year two five)

chosenClaimsFive :: Int -> Int -> String
chosenClaimsFive year five
  | five > 2  = ">2"
  | five == 1 = show year <> "Y"
  | otherwise = show five

chosenClaimsTwo :: Int -> Int -> Int -> String
chosenClaimsTwo year two five
  | chosenClaimsFive year five == ">2" = ">=0"
  | two == 1 && (five == 1 || five == 2) = "1"
  | two == 0 = "0"
  | otherwise = ">0"

