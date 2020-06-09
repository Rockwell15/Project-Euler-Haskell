
-- answer is the sum of diagonal pattern in a 1001x1001 grid
answer = (1 +) . sum . takeWhile (\x -> x <= limit) $ diagonalPattern 1 2
  where limit = 1001 * 1001

-- diagonalPattern is ( i=1, n=2)
-- * i + n, 4 times
-- * n += 2, restart at above line
diagonalPattern :: Int -> Int -> [Int]
diagonalPattern start inc = sequence ++ diagonalPattern (last sequence) (inc+2)
      where sequence = [ i * inc + start | i <- [1..4] ]
