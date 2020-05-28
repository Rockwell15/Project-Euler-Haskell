isPrime :: Integer -> Bool
isPrime x
  | x == 1 = False
  | x == 2 = True
  | x `mod` 2 == 0 = False
  | otherwise = False == any (\y -> x `mod` y == 0) [3,5..max]
    where max = floor . sqrt . fromInteger $ x

answer = sum [ x | x <- [1..2000000], isPrime x ]