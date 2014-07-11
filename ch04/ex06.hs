mult' :: (Num a) => a -> a -> a -> a
mult' x y z = x * y * z

mult'' :: (Num a) => a -> a -> a -> a
mult'' = \x -> \y -> \z -> x * y * z

main = do
    putStrLn . show $ mult' 1 2 3
    putStrLn . show $ mult'' 1 2 3
