module HW1.HW1
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  , hanoi
  ) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = x' : toDigitsRev xs
  where
    x' = x `rem` 10
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
validate = (==) 0 . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 from target _ = [(from, target)]
hanoi n from target storage = stash ++ ((from, target) : back)
  where
    n' = max 0 (n - 1)
    back = hanoi n' storage target from
    stash = hanoi n' from storage target

reve :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
reve 0 _ _ _ _ = []
reve 1 from target _ _ = [(from, target)]
reve n from target storage storage2 =
  reve (n - k) from storage target storage2 ++ hanoi k from target storage2
  where
    k = div n 2
