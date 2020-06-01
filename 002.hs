import Extras

-- answer is the sum of the even-valued fibonacci terms, up to 4m
answer = sum [ x | x <- filter even $ takeWhile (< 4000000) $ map fibonacci [1..] ]
