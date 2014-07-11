and1 :: Bool -> Bool -> Bool
and1 a b = if not a then False else
    if b then True else False

and2 :: Bool -> Bool -> Bool
and2 a b = if a then
    if b then True else False
    else False

test f = [f x y | x <- [True, False], y <- [True, False]]

main = do
    putStrLn . show $ test and1
    putStrLn . show $ test and2
