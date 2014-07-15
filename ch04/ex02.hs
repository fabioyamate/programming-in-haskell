safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

safetail' :: [a] -> [a]
safetail' xs
        | null xs = xs
        | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

main = do
    print $ safetail [1,2,3,4]
    print $ (safetail [] :: [Int])
    print $ safetail' [1,2,3,4]
    print $ (safetail' [] :: [Int])
    print $ safetail'' [1,2,3,4]
    print $ (safetail'' [] :: [Int])
