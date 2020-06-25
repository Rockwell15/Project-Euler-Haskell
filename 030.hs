-- NOTES
-- need bounds to make program computable
-- low  is 2**5 * 2
-- high is ( the first x where 9**5 * x > 9 repeated x times ) - 1

-- answer is the sum of all numbers that can be written as the sum of 5th powers of their digits
answer = sum [ x | x <- [low..high], isValid x ]

isValid n = n ==  sum [ ( read [x] ) ^ 5 | x <- show $ n ]

low = 2 ^ 5 * 2
high = 9 ^ 5 * ( ( findx 2 ) - 1 )
  where findx x
          | 9 ^ 5 * x < ( read $ take x $ repeat '9' ) = x
          | otherwise = findx $ x + 1
