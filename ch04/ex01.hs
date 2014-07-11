halve xs = (take half xs, drop half xs)
    where half = (length xs) `div` 2

main = do
    putStrLn . show $ halve [1,2,3,4,5,6]
