module OrdGood where

import Data.Monoid ((<>))

-- Typeclasses to the rescue!
-- We need to derive Eq because Ord is a "sub-type" of it
-- Haskell can infer how to derive it, because it rocks.
-- Creating an instance of Ord looks like a lot of work, but it will pay
-- off!

data Color = V | G | R deriving (Eq, Show)

data QuoteData = QuoteData { province :: String, isBersani :: Bool, isNewInsurance :: Bool, chosenClaimsExperience5 :: Int, chosenClaimsExperience2 :: Int, chosenClaimsExperienceYear :: Int }

quicky = QuoteData{province="MI", isBersani=False, isNewInsurance=False, chosenClaimsExperience5=1, chosenClaimsExperience2=1, chosenClaimsExperienceYear=2}

instance Ord Color where
    compare V V = EQ
    compare G G = EQ
    compare R R = EQ
    compare V _ = GT
    compare G V = LT
    compare G _ = GT
    compare R _ = LT

-- There's something odd about this...
-- Why am I putting all colors in a list and then folding?
-- Colors can compose! we need a Monoid
-- But what's a Monoid?

restrict :: QuoteData -> Color
restrict quote = minimum [bersani quote, isRedProvince quote, chosenClaimsExperience quote]

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

