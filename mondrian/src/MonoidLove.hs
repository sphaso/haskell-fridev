module MonoidLove where

import Data.Monoid

-- What if all we needed was some Monoid Love?
-- We represent our domain as an associative structure
-- Monoid is perfect!

data Color = V | G | R deriving (Eq, Show)

data QuoteData = QuoteData { province :: String, isBersani :: Bool, isNewInsurance :: Bool, chosenClaimsExperience5 :: Int, chosenClaimsExperience2 :: Int, chosenClaimsExperienceYear :: Int }

newtype Restrictive = Restrictive Color deriving (Show)
newtype Widening    = Widening Color deriving (Show)

instance Ord Color where
    compare V V = EQ
    compare V _ = GT
    compare G G = EQ
    compare G V = LT
    compare G _ = GT
    compare R R = EQ
    compare R _ = LT

instance Monoid Restrictive where
    mempty = Restrictive V
    mappend (Restrictive a) (Restrictive b) = Restrictive $ min a b

instance Monoid Widening where
    mempty = Widening R
    mappend (Widening a) (Widening b) = Widening $ max a b

restrict :: QuoteData -> Restrictive
restrict quote = bersani quote
               <> isRedProvince quote
               <> chosenClaimsExperience quote

widen quote = chosenClaimsExperience quote

-----------------------------------------------------------------------------------
    -------------------------------------------------------------------------------
        ---------------------------------------------------------------------------

bersani :: QuoteData -> Restrictive Color
bersani QuoteData{isBersani=True} = G
bersani _ = V

redProvince :: [String]
redProvince = ["NA", "CE", "SA", "BA", "BT", "FG", "TA", "VV", "CZ", "LT", "PO", "AV", "BR", "KR", "RC"]

isRedProvince :: QuoteData -> Restrictive Color
isRedProvince QuoteData{province=prov} =  case prov `elem` redProvince of
                                            True -> V
                                            _    -> R

chosenClaimsExperience :: QuoteData -> Widening Color
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

