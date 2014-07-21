sumsqr :: Int -> Int
sumsqr n = sum [x^2 | x <- [1..n]]

sumsqr' :: Int -> Int
sumsqr' n = sum (map (\x -> x * x) [1..n])

sumsqr'' :: Int -> Int
sumsqr'' n = foldl (\acc x -> acc + x^2) 0 [1..n]

main = do
    print $ sumsqr 100
    print $ sumsqr' 100
    print $ sumsqr'' 100
