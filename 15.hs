answer size = memo_paths size size

memo_paths :: Int -> Int -> Int
memo_paths x y = v x y
  where v x y = a !! x !! y
        a = map (\i -> map (paths i) [0..]) [0..]
        paths 0 _ = 1
        paths _ 0 = 1
        paths x y = v (x-1) y + v x (y-1)
