last xs = xs !! (length xs - 1)

-- last2 xs = head (reverse xs)
last2 = head . reverse
