last' :: [x] -> x
last' xs = xs !! (length xs - 1)

last'' :: [x] -> x
-- last'' xs = head (reverse xs)
last'' = head . reverse
