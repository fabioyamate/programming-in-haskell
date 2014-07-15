find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Enum b, Eq a, Num b) => a -> [a] -> [b]
positions c xs = find c (zip xs [0..])

main = do
    print $ positions 'a' "aadshaldajkbas"
