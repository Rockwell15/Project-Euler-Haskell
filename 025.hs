import Extras
import Data.List

-- answer is the first fibonacci sequence to hit 1000 digits
answer = 1 + last $ takeWhile (\ x -> 1000 > ( digitCount ( fibonacci x ) ) ) [1..]


