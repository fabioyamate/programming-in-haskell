power :: (Eq a, Num a, Ord a) => a -> a -> a
power _ 0 = 1
power a n
        | n < 0 = error "only non-negative number"
        | otherwise = a * power a (n - 1)

-- power 2 3
-- = { applying power }
-- 2 * power 2 2
-- = { applying power }
-- 2 * 2 * power 2 1
-- = { applying power }
-- 2 * 2 * 2 * power 2 0
-- = { applying power }
-- 2 * 2 * 2 * 1
-- = { applying * }
-- 8

main = do
    print $ 2 `power` 0
    print $ 2 `power` 3
    print $ 2 `power` (-3)
