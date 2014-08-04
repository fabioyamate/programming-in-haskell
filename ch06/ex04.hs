merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
        | x <= y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

main = do
    print $ merge [2,5,6] [1,3,4]
    print $ merge [1,2,3] [4,5,6]
