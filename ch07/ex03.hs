map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr selector []
    where selector x acc = if f x then x : acc
                           else acc

main = do
    print $ map (*2) input == map' (*2) input
    print $ filter odd input == filter' odd input
    where input = [1,2,3,4]
