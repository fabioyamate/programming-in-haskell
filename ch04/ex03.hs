or1 :: Bool -> Bool -> Bool
or1 True _ = True
or1 _ True = True
or1 _ _ = False

or2 :: Bool -> Bool -> Bool
or2 False a = a
or2 a _ = a

or3 :: Bool -> Bool -> Bool
or3 False False = False
or3 _ _ = True

or4 :: Bool -> Bool -> Bool
or4 True True = True
or4 True False = True
or4 False True = True
or4 False False = False

test f = [f x y | x <- [True, False], y <- [True, False]]

main = do
    putStrLn . show $ test or1
    putStrLn . show $ test or2
    putStrLn . show $ test or3
    putStrLn . show $ test or4
