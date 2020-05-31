import Data.Word

-- answer is longest collatz sequence in numbers 1 -> 1e6
answer n = snd $ maximum [ ( collatzLen x, x ) | x <- [1..n-1] ]

main = print $ answer 1000000

collatzLen :: Int -> Int
collatzLen = (map coll [0..] !!)
  where
    coll x
      | x < 2 = x
      | even x = 1 + collatzLen (x `div` 2)
      | odd x  = 1 + collatzLen (x*3 + 1)

collatzLen :: Int -> Int
collatzLen 1 = 1
collatzLen n | n `mod` 2 == 0 = 1 + ( collatzLen $ n `div` 2 )
               | otherwise      = 1 + ( collatzLen $ 3*n+1 )
