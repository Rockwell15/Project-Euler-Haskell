-- answer is the sum of all digits in the number 2^1000
answer = sum [ read [x] | x <- show $ 2 ^ 1000 ]
