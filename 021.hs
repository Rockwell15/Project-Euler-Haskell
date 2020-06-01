import Extras

-- answer is the sum of amicable numbers under 1000
answer = sum $ amicable 10000

-- amicable returns unique numbers, which equal each other after getting the sum of their divisors
amicable n = [ x | x <- [ 2..n ], let ds = divisorSum x, ds /= x && x == divisorSum ds ]

divisorSum x = sum . init $ divisors x
