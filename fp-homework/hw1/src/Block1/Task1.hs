module Block1.Task1 where

newtype DayOfWeek = FromNat Int

instance Show DayOfWeek where
  show (FromNat 0) = "Monday"
  show (FromNat 1) = "Tuesday"
  show (FromNat 2) = "Wednesday"
  show (FromNat 3) = "Thursday"
  show (FromNat 4) = "Friday"
  show (FromNat 5) = "Saturday"
  show (FromNat 6) = "Sunday"
  show (FromNat _) = error "there's only 7 days of week"

instance Eq DayOfWeek where
  (FromNat m) == (FromNat n) = n == m

afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays (FromNat n) m = FromNat ((m + n) `mod` 7)

nextDay :: DayOfWeek -> DayOfWeek
nextDay d = afterDays d 1

isWeekend :: DayOfWeek -> Bool
isWeekend (FromNat 5) = True
isWeekend (FromNat 6) = True
isWeekend _ = False

daysToParty :: DayOfWeek -> Int
daysToParty (FromNat n) = mod (11 - n) 7
