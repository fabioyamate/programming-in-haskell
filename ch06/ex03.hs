and' :: [Bool] -> Bool
and' [] = False
and' (x:xs) = x && and xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "out of bounds"
(!!!) (x:_) 0 = x
(!!!) (_:xs) n = xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
    | n == x = True
    | otherwise = elem' n xs

main = do
    print $ and' [True, False]
    print $ and' [True, True]
    print $ concat' [[1,2,3], [4,5,6]]
    print $ replicate' 3 True
    print $ [3,2,1] !!! 2
    print $ elem' 1 [3,2,1]
    print $ elem' 1 [3]
