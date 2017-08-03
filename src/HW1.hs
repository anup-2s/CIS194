module HW1
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  ) where

toDigits :: Integer -> [Integer]
toDigits x
  | x < 10 = [x]
  | otherwise = x' : toDigits xs
  where
    x' = x `mod` 10
    xs = x `div` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = snd . foldr helper (False, [])
  where
    helper :: Integer -> (Bool, [Integer]) -> (Bool, [Integer])
    helper x (True, xs) = (False, 2 * x : xs)
    helper x (False, xs) = (True, x : xs)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (==) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits
