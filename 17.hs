-- answer is sum of letters in each number 1 -> 1000
-- the letter count is:
--   11 if 1000
--   ( 3rd digit letters + hundred(7) )
--   + "and" iff both above AND below apply
--   + | remainder < 10 = single digit mapping
--     | remainder < 14 = preteens mapping
--     | 0 < remainder  = single digits mapping + tens mapping
-- ** 15 && eighteen are special cases
-- ** its acually simplest to combine singles && preteens

answer = sum [ letters d | d <- [1..1000] ]

letters n = first ( div n 100 ) + second ( mod n 100 ) + and
  where 
    first x
      | 10 == x   = 11
      | 0 < x     = singles !! x + 7
      | otherwise = 0
    second x
      | x < 14    = singles !! x
      | x == 15   = 7
      | x == 18   = 8
      | otherwise = singles !! ( mod x 10 ) + tens !! ( div x 10 )
    and = if div n 100 > 0 && mod n 100 > 0 then 3 else 0

-- 
singles = [ 0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8 ]
tens = [ 0, 4, 6, 6, 5, 5, 5, 7, 6, 6 ]
