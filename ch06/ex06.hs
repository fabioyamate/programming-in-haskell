sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (_:xs) = last' xs

last'' :: [a] -> a
last'' [] = error "empty list"
last'' (x:xs)
    | null xs = x
    | otherwise = last'' xs

main = do
    print $ sum' []
    print $ sum' [1..10]
    print $ take' 0 [1..10]
    print $ take' 2 [1..10]
    print $ last' [1..10]
    print $ last'' [1..10]
