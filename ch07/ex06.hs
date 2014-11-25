curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x -> \y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

main = do
    print $ f 1 2
    where f = curry' $ uncurry' (+)
