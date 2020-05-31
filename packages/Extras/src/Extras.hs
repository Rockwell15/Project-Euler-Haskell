module Extras (digitSum, divisors, divisorCount) where

digitSum n = sum [ read [x] | x <- show $ n ]

divisors :: Int -> [Int]
divisors x = [ n | n <- [1..x], x `mod` n == 0 ]

divisorCount :: Int -> Int
divisorCount x = sum [ 1 | n <- divisors $ floor $ sqrt $ fromIntegral x ] * 2
