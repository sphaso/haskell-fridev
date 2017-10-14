module FunctorFunk where

import Data.Monoid ((<>))

-- What if everything we know is wrong?
-- Maybe foldable was hiding something from us...
-- Let's go back to Ord and refactor to discover
-- the funky Functor!

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

widen :: QuoteData -> Color
widen quote = maximum $ fmap ($ quote) [chosenClaimsExperience]

restrict :: QuoteData -> Color
restrict quote = minimum $ fmap ($ quote) [bersani, isRedProvince]

calculate :: QuoteData -> Color
calculate quote = maximum $ fmap ($ quote) [restrict, widen]

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

