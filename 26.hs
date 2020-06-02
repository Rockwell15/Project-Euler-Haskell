import Data.List
import Data.Maybe

-- TODO: optimize

-- answer is the integer which has the longest recurring cycle when dividing 1 by it
answer = snd $ maximum [ ( recurringlength x, x ) | x <- [1..999] ]

-- is the length of the recurring sequence in 1 / x
recurringlength x = if (dlist!!firstrepeat == 0) then 0 else firstrepeat - ( fromMaybe 0 firsti )
  where
    (firsti, firstrepeat) = fromMaybe (Nothing,0) $ find (\(fi, si) -> Nothing /= fi ) maprepeats
    maprepeats = map (\i -> ( check i dlist, i ) ) [1..]
    dlist = decimals x

check i dlist = find (\p -> dlist!!i == dlist!!p ) [1..i-1]

-- is the potentially infinite map of all decimals in 1 / x
decimals x = scanl (\acc _ -> ( mod acc x ) * 10 ) 1 [1..]



-- wrote the logic in JS first to wrap my mind around it:
-- rs = x => {
--     var d = 1 / x
--     var r = 1 % x
--     var past = []
--     var xx = 0

--     while ( r && xx++ < 10 ) {
--         past.push( r )

--         r = r % x
--         r *= 10

--         if ( -1 < past.indexOf( r ) ) return past.length - past.indexOf( r ) 
--     }


--     return 0
-- }
