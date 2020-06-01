import Extras

-- answer is the sum of all posi ints which cannot be written as the sum of abundant numbers
answer = sum [ x | x <- [1..28123], not $ elem x abundantSums ]

-- `abundantSums` is the collection of numbers created from the sum of 2 abundant numbers below 28123
abundantSums = [ x + y | x <- abundantnumbers, y <- abundantnumbers ]

abundantnumbers = [ x | x <- [1..28123], x < divisorSum x - x ]
