import Data.Maybe
import Data.List
import Extras

solve = do
  x <- readFile "data/p022_names.txt"
  let namelist = explode ',' $ removechar '"' x
  return $ answer namelist

-- answer is the sum of all `name_score`s in the sorted list, names
answer :: [[Char]] -> Int
answer namelist = sum [ i * name_score x | ( i, x ) <- zip [1..] $ sort namelist ]

-- name_score is the sum of all characters in name
name_score :: [Char] -> Int
name_score name = sum [ fromJust $ lookup c charmap | c <- name ]

charmap :: (Num b, Enum b) => [(Char, b)]
charmap = zip ['A'..'Z'] [1..]
