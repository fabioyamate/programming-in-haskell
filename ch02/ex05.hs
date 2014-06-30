init' :: [a] -> [a]
init' = reverse . tail . reverse
-- init' xs = reverse (tail (reverse xs))

init'' :: [a] -> [a]
init'' xs = take (n - 1) xs
    where
        n = length xs

-- main = do
--     putStrLn . show $ init' [1..20]
--     putStrLn . show $ init'' [1..20]
