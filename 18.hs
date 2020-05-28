-- solve bottom up so we start with space == breadth of tree && elim 1 each loop
-- answer is the final value of the array, after recursively choosing the greatest total
-- -- total is (p)arent node value + greatest (c)hild value
-- -- array is the tree's leaf nodes

answer = do
  x <- readFile "data/18.txt"
  let result = map (\l -> map (\x -> read x ::Int ) $ words l ) $ lines x
  let final = solve result
  return final

solve array = ( foldr1 reduce array ) !! 0
  where
    reduce p c = [ p!!x + max (c!!x) (c!!(x+1)) | x <- [0..length p-1] ]
