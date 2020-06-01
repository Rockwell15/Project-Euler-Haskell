module Extras (digitSum, divisors, divisorCount, divisorSum, removechar, explode) where

-------------
-- Numbers --
-------------

digitSum n = sum [ read [x] | x <- show $ n ]

divisors :: Int -> [Int]
divisors x = [ n | n <- [1..x], x `mod` n == 0 ]

divisorSum :: Int -> Int
divisorSum x = sum $ divisors x

divisorCount :: Int -> Int
divisorCount x = sum [ 1 | n <- divisors $ floor $ sqrt $ fromIntegral x ] * 2

-------------
-- Strings --
-------------

removechar :: Char -> String -> String
removechar c string = [ x | x <- string, c /= x ]

explode :: Char -> String -> [String]
explode delim string =
  foldr
  (\c acc ->
    if c == delim 
      then "":acc 
      else (( [c] ++ head acc ):tail acc )
  )
  [""]
  string
