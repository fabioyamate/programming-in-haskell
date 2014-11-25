all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) = f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x:xs

main = do
    print $ all odd [1,3,5] == all' odd [1,3,5]
    print $ any even [1,3,5] == any' even [1,3,5]
    print $ takeWhile even [1,2,3,4,5] == takeWhile' even [1,2,3,4,5]
    print $ dropWhile even [1,2,3,4,5] == dropWhile' even [1,2,3,4,5]
