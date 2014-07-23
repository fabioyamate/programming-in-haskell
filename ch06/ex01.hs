power :: (Eq a, Num a, Ord a) => a -> a -> a
power _ 0 = 1
power a n
        | n < 0 = error "only non-negative number"
        | otherwise = a * power a (n - 1)

main = do
    print $ 2 `power` 0
    print $ 2 `power` 3
    print $ 2 `power` (-3)
