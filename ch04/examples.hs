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
    putStrLn . show $ abs' (-3)
    putStrLn . show $ not' True
    putStrLn . show $ and' True True
    putStrLn . show $ and' False True
    putStrLn . show $ and''' True True
    putStrLn . show $ and''' True False
    putStrLn . show $ 1:(2:[])
    putStrLn . show $ (\x -> x + x) 2
    putStrLn . show $ (\x -> \y -> x + y) 1 2
    putStrLn . show $ const 1 2
