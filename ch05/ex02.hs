replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

replicate'' :: Int -> a -> [a]
replicate'' n x = map (const x) [1..n]

main = do
    print $ replicate' 3 True
    print $ replicate'' 3 True
