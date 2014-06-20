init :: [a] -> [a]
init = reverse . tail . reverse
-- init xs = reverse (tail (reverse xs))

init2 xs = take (n - 1) xs
    where
        n = length xs

-- main = do
--     putStrLn . show $ init2 [1..20]
