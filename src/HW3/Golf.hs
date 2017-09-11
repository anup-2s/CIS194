module HW3.Golf where

import Data.List.Split (chunksOf)

nth :: [a] -> Int -> [a]
nth xs n = map head . chunksOf n . drop n $ xs

skips :: [a] -> [[a]]
skips xs = map (nth xs) [1 .. length xs]
