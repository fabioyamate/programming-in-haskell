abs' :: (Num a, Ord a) => a -> a
abs' n
    | n >= 0 = n
    | otherwise = -n

not' :: Bool -> Bool
not' False = True
not' True = False

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

and'' :: Bool -> Bool -> Bool
and'' True b = b
and'' False _ = False

and''' :: Bool -> Bool -> Bool
and''' b c
        | b == c = b
        | otherwise = False

const' :: a -> (b -> a)
const' x = \_ -> x

main = do
    print $ abs' (-3)
    print $ not' True
    print $ and' True True
    print $ and' False True
    print $ and''' True True
    print $ and''' True False
    print $ 1:(2:[])
    print $ (\x -> x + x) 2
    print $ (\x -> \y -> x + y) 1 2
    print $ const 1 2
