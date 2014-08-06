unfold p h t x
        | p x = []
        | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

-- map' (*2) [1,2]
-- { applying unfold }
-- (*2) (head [1,2]) : unfold null (*2) tail (tail [1,2])
-- { applying unfold }
-- (*2) (head [1,2]) : (*2) (head [2]) : unfold null (*2) tail (tail [2])
-- { applying unfold }
-- (*2) (head [1,2]) : (*2) (head [2]) : []
-- [2,4]

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

-- iterate' (*2) 1
-- { applying unfold }
-- id 1 : unfold (const False) id (*2) (1*2)
-- { applying unfold }
-- id 1 : id 2 : unfold (const False) id (*2) (2*2)
-- { applying unfold }
-- id 1 : id 2 : id 4 : unfold (const False) id (*2) (4*2)
-- ...

main = do
    print $ chop8 [1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0] == chop8' [1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0]
    print $ map (*2) [1,2,3,4,5] == map' (*2) [1,2,3,4,5]
    print $ take 3 (iterate (^2) 2) == take 3 (iterate' (^2) 2)
    print $ unfold (==0) (`mod` 2) (`div` 2) 5
