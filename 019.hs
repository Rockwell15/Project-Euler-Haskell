-- answer is the number of sundays that fall on the first of the month from year 1901 to 2000
answer = sum [ sundays_on_first year | year <- [1901..2000] ]

-- number of sundays which fell on the first in `year`
sundays_on_first year = sum [ is_sunday $ start_day year + x | x <- get_months year ]

-- starting day of `year`
start_day year = ( year * 365 + leap_days year + start1ad ) `mod` 7

-- leap years from 0 -> `year`
leap_days year = div year 4 - div year 100 + div year 400

-- helpers
is_sunday x  = if 0 == mod x 7 then 1 else 0
get_months x = if ( 0 == mod x 4 && ( 0 == mod x 400 || 0 < mod x 100 ) ) then monthsL else months

-- constants
start1ad = 6 -- was a saturday
months  = scanl1 (+) [0,31,28,31,30,31,30,31,31,30,31,30]
monthsL = scanl1 (+) [0,31,29,31,30,31,30,31,31,30,31,30]

-- pretty brute. how to improve?
-- each year shifts days by 1 (if may 4th is Mon this year, will be tues next)
-- leap year shifts days by 2
-- is leap year if :: 0 == mod x 400 || ( 0 < mod x 100 && 0 == mod x 4 )
-- 1 jan 1901 -> Tuesday
