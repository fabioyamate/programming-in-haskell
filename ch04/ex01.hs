halve xs = (take half xs, drop half xs)
    where half = (length xs) `div` 2

halve' xs = splitAt half xs
    where half = (length xs) `div` 2

main = do
    print $ halve [1,2,3,4,5,6]
    print $ halve' [1,2,3,4,5,6]
