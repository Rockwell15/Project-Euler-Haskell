import Data.List (nub)

-- answer is distinct terms in the combos
answer = length $ nub combos

-- combos is the list of numbers a^b where 2 <= a <= 100 && 2 <= b <= 100
combos = [ a ^ b | a <- [2..100], b <- [2..100] ]
