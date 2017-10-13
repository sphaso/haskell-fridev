module MonoidLove where

data Color = V | G | R deriving (Eq)

data QuoteData = QuoteData { province :: String }

newtype Restrictive = Restrictive Color
newtype Widener     = Widener Color

instance Ord Color where
    compare V V = EQ
    compare V _ = GT
    compare G G = EQ
    compare G V = LT
    compare G _ = GT
    compare R R = EQ
    compare R _ = LT

instance Monoid Restrictive where
    mempty = Restrictive R
    mappend (Restrictive a) (Restrictive b) = case compare a b of
                                                LT -> Restrictive a
                                                _  -> Restrictive b

instance Monoid Widener where
    mempty = Widener V
    mappend (Widener a) (Widener b) = case compare a b of
                                        GT -> Widener a
                                        _  -> Widener b

isBersani QuoteData -> Restrictive
isBersani QuoteData{isBersani=True} = Restrictive G
isBersani _ = Restrictive V

redProvince :: [String]
redProvince = ["NA", "CE", "SA", "BA", "BT", "FG", "TA", "VV", "CZ", "LT", "PO", "AV", "BR", "KR", "RC"]

isRedProvince :: QuoteData -> Restrictive
isRedProvince QuoteData{province=prov} =  case prov `elem` redProvince of
                                            True -> Restrictive V
                                            _    -> Restrictive R

chosenClaimsExperience :: QuoteData -> Restrictive
chosenClaimsExperience QuoteData{is_new_insurance=True} = Restrictive V
chosenClaimsExperience QuoteData{chosenClaimsExperience5=five, chosenClaimsExperience2=two, chosenClaimsExperienceYear=claimsYear} 
  | stringa == "0-0" = Restrictive V
  | stringa == "5Y-0" = Restrictive V
  | stringa == "4Y-0" = Restrictive V
  | stringa `elem` ["3Y-0", "2Y-1", "1Y-1", "2-0", "2-1", "2-2", "2->0", ">2->=0"] = Restrictive R
  | otherwise = Restrictive G
   where sringa = (chosenClaimsFive year five) <> (chosenClaimsTwo two five)

chosenClaimsFive :: String -> Int -> String
chosenClaimsFive year five
  | five > 2  = ">2"
  | five == 1 = year <> "Y"
  | otherwise = show five

chosenClaimsTwo :: Int -> Int -> String
chosenClaimsTwo two five
  | chosenClaimsFive five == ">2" = ">=0"
  | two == 1 && (five == 1 || five == 2) = "1"
  | two == 0 = "0"
  | otherwise = ">0"
