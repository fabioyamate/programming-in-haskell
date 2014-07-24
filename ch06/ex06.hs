sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs) = x : take (n - 1) xs

last' :: [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (_:xs) = last' xs

main = do
    print $ sum' []
    print $ sum' [1..10]
    print $ take' 2 [1..10]
    print $ last' [1..10]
