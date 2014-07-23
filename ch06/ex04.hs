merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge (x:y:xs) []
        | x <= y = x : merge [y] xs
        | otherwise = y : merge [x] xs
merge [] (x:y:xs)
        | x <= y = x : merge [y] xs
        | otherwise = y : merge [x] xs
merge (x:xs) []
    | null xs = [x]
    | otherwise = x : merge [] xs
merge [] (x:xs)
    | null xs = [x]
    | otherwise = x : merge [] xs
merge (x:xs) (y:ys)
        | x <= y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

main = do
    print $ merge [2,5,6] [1,3,4]
