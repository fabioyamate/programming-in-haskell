map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

map' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr selector []
    where selector x acc = if f x then x : acc
                           else acc

main = do
    print $ map' (*2) [1,2,3,4]
    print $ filter' odd [1,2,3,4]
