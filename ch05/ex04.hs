factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
    where isPerfect n' = sum (factors n') - n' == n'

-- using filter but not so good
perfects' :: Int -> [Int]
perfects' n = [x | x <- [1..n], isPerfect x]
    where isPerfect n' = sum (filter (/= n') (factors n')) == n'

perfects'' :: Int -> [Int]
perfects'' n = [x | x <- [1..n], isPerfect x]
    where isPerfect n' = sum (init (factors n')) == n'

perfects''' :: Int -> [Int]
perfects''' n = [x | x <- [1..n], isPerfect x]
    where isPerfect n' = sumFactors n' == n'
          sumFactors = sum . init . factors

main = do
    print $ perfects 500
    print $ perfects' 500
    print $ perfects'' 500
    print $ perfects''' 500
