-- step 1: define the type
-- step 2: enumerate the cases
-- step 3: define the simple cases
-- step 4: define the other cases
-- step 5: generalise and simplify

-- step 1:
--      sum' :: Num a => [a] -> a
-- step 2:
--      sum' [] =
--      sum' (x:xs) =
-- step 3 & 4:
--      sum' [] = 0
--      sum' (x:xs) = x + sum xs
-- step 5:
--      sum' = foldr (+) 0

sum' :: [Integer] -> Integer
sum' = foldr (+) 0

-- step 1:
--      take' :: Int -> [a] -> [a]
-- step 2:
--      take' 0 [] =
--      take' 0 xs =
--      take' n [] =
--      take' n (x:xs) =
-- step 3 & 4:
--      take' 0 [] = []
--      take' 0 xs = []
--      take' n [] = []
--      take' n (x:xs) = x : take' (n - 1) xs
-- step 5:
--      take' _ [] = []
--      take' 0 _ = []
--      take' n (x:xs) = x : take' (n - 1) xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

-- step 1:
--      last' :: [a] -> a
-- step 2:
--      last' (x:xs) =
-- step 3 & 4:
--      last' (x:xs)
--          | null xs = x
--          | otherwise = last' xs
-- step 5:

last' :: [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (_:xs) = last' xs

last'' :: [a] -> a
last'' [] = error "empty list"
last'' (x:xs)
    | null xs = x
    | otherwise = last'' xs

main = do
    print $ sum' []
    print $ sum' [1..10]
    print $ take' 0 [1..10]
    print $ take' 2 [1..10]
    print $ last' [1..10]
    print $ last'' [1..10]
