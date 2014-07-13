concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- guards on list comprehensions
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- so nice
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- sorted has the invariant that the next is always greater or equal than the previous
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

main = do
    print $ concat' ["abc", "def", "ghi"]
    print $ length' ["abc", "def", "ghi"]
    print $ factors 120
    print $ prime 7
    print $ primes 7
    print $ sorted [1,2,3,4]
