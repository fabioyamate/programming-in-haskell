-- just invert concat with larger first and smaller later
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
    where smaller = [n | n <- xs, n <= x]
          larger  = [n | n <- xs, n > x]

-- main = do
--   putStrLn . show $ rqsort [4,1,2,3]
