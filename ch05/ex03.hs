pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- ns, y <- ns, z <- ns, x^2 + y^2 == z^2]
    where ns = [1..n]

main = do
    print $ pyths 10
