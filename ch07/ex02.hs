all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (x:xs) = f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = f x || any' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f xs = [x | x <- xs, f x]

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f xs = [x | x <- xs, (not . f) x]

main = do
    print $ all' odd [1,3,5]
    print $ any' even [1,3,5]
    print $ takeWhile' even [1,2,3,4,5]
    print $ dropWhile' even [1,2,3,4,5]
