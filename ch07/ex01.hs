main = do
    print $ [f x | x <- xs, p x] == map f (filter p xs)
    where f = (*2)
          p = odd
          xs = take 5 [1..]
