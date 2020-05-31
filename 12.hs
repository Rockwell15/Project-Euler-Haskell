import Extras

-- answer is first triangle number where it has > 500 divisors
answers = head [ x | x <- scanl1 (+) [1..], 500 < divisorCount x ]
