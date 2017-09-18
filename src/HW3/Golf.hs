{-# LANGUAGE TupleSections #-}

module HW3.Golf where

import Data.List.Split (chunksOf)
import Data.Map.Lazy
       (fromListWith, toList, Map, lookup, foldrWithKey, insert, empty)

nth :: [a] -> Int -> [a]
nth xs n = map head . chunksOf n . drop n $ xs

skips :: [a] -> [[a]]
skips xs = map (nth xs) [1 .. length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:x)
  | b > a && b > c = b : localMaxima (c : x)
  | otherwise = localMaxima (c : x)
localMaxima _ = []

d' k v m
  | v <= 1 = m
  | otherwise = insert k (v - 1) m

d :: Map Int Int -> String -> (String, Map Int Int)
d m s = (s, foldrWithKey d' empty m)

r'' :: Map Int Int -> String
r'' m = concatMap (maybe " " (const "*") . (`Data.Map.Lazy.lookup` m)) [0 .. 9]

r''' :: (String, Map Int Int) -> ([String], Map Int Int)
r''' (s, m) = (s : fst (r' m), m)

r' :: Map Int Int -> ([String], Map Int Int)
r' map
  | null map = (["========", "0123456789"], map)
  | otherwise = r''' . d map . r'' $ map

r :: Map Int Int -> String
r =  unlines . fst . r'

histogram = r . fromListWith (+) . map (, 1)
