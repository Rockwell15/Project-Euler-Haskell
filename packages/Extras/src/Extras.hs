module Extras (digitSum) where

digitSum n = sum [ read [x] | x <- show $ n ]

divisorCount :: Int -> Int
divisorCount x = sum [ 1 | n <- [1..floor $ sqrt $ fromIntegral x], x `mod` n == 0 ] * 2
