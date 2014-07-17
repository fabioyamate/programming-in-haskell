main = do
    print $ a == b
    where
        a = [(x, y) | x <- [1,2,3], y <- [4,5,6]]
        -- I think that I'm cheating on this answer...
        b = [x | x <- concat [[(y, 4), (y, 5), (y, 6)] | y <- [1,2,3]]]
