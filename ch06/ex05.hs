-- Merge Sort with merge already implemented is trivial to do.
-- However, one problem that I got stuck was the case of sorting
-- a list with a single element, it was in infinite recursion since
-- it always pattern matched the `msort xs` case.

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge (msort left) (msort right)
    where (left, right) = halve xs

msort' :: Ord a => [a] -> [a]
msort' [] = []
msort' (x:xs)
    | null xs = [x]
    | otherwise =  merge (msort' left) (msort' right)
    where (left, right) = halve (x:xs)

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take half xs, drop half xs)
    where half = length xs `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
        | x <= y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

main = do
    print $ msort ([] :: [Int])
    print $ msort [1]
    print $ msort [1..100]
    print $ msort' [1..100]
