{-# LANGUAGE TupleSections #-}

module HW3.Golf where

import Data.List.Split (chunksOf)
import qualified Data.Map.Lazy as M

nth :: [a] -> Int -> [a]
nth xs n = map head . chunksOf n . drop n $ xs

skips :: [a] -> [[a]]
skips xs = map (nth xs) [1 .. length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:x)
  | b > a && b > c = b : localMaxima (c : x)
  | otherwise = localMaxima (c : x)
localMaxima _ = []

createMap :: [Int] -> M.Map Int Int
createMap = M.fromListWith (+) . map (, 1)

reduceVals' :: Int -> Int -> M.Map Int Int -> M.Map Int Int
reduceVals' k v m
  | v <= 1 = m
  | otherwise = M.insert k (v - 1) m

reduceVals :: M.Map Int Int -> M.Map Int Int
reduceVals = M.foldrWithKey reduceVals' M.empty

keysToValues :: M.Map Int Int -> String
keysToValues m = concatMap (maybe " " (const "*") . flip M.lookup m) [0 .. 9]

getCounts :: [String] -> M.Map Int Int -> [String]
getCounts s m
  | null m = s
  | otherwise = getCounts (keysToValues m : s) (reduceVals m)

appendBase :: [String] -> [String]
appendBase = (++ ["==========", "0123456789"])

histogram = unlines . appendBase . getCounts [] . createMap
