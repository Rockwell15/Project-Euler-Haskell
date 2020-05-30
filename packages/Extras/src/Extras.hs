module Extras (digitSum) where

digitSum n = sum [ read [x] | x <- show $ n ]
