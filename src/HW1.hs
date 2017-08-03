module HW1
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  ) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = x' : toDigitsRev xs
  where
    x' = x `mod` 10
    xs = x `div` 10

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = snd . foldr helper (False, [])
  where
    helper :: Integer -> (Bool, [Integer]) -> (Bool, [Integer])
    helper x (True, xs) = (False, 2 * x : xs)
    helper x (False, xs) = (True, x : xs)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev

validate :: Integer -> Bool
validate = (==) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from target _ = [(from, target)]
hanoi n from target storage = back ++ ((from, target) : stash)
  where
    n' = n - 1
    back = hanoi n' storage target from
    stash = hanoi n' from storage target
