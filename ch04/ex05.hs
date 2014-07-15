and1 :: Bool -> Bool -> Bool
and1 a b = if a then b else False

and2 :: Bool -> Bool -> Bool
and2 a b = if a then b else a

test f = [f x y | x <- [True, False], y <- [True, False]]

main = do
    print $ test and1
    print $ test and2
