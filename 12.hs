
-- answer is first triangle number where it has > 500 divisors
answers = head [ x | x <- scanl1 (+) [1..], 500 < divisorCount x ]

divisorCount :: Int -> Int
divisorCount x = sum [ 1 | n <- [1..floor $ sqrt $ fromIntegral x], x `mod` n == 0 ] * 2
