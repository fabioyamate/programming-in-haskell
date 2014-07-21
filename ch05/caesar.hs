-- Caesar cipher

module CaesarCipher where
import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

-- First some consideration:
--
-- -3 `mod` 26 is not (-3) `mod` 26
--
-- the infix notation has precedence, so the first expression is the same as:
-- -(3 `mod` 26) = -3
--
-- while the later is 23
--
-- Now the trick here is why (-3) % 26 is 23?
--
-- It applies the same method as mod does (which is the reminder), but for negative
-- numbers it may not be so clear.
--
-- 26 * (-1) = -26, and (-3) - (-26) = 23
--
-- so for (-40) `mod` 26
--
-- 26 * (-2) = -52, and (-40) - (-52) = 12
--
-- So, with this property it makes easy to create a circular list
--
-- (-neg)      0     (+pos)
-- [6,5,4,3,2][1,2,3,4,5,6]
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

-- this makes clear that building small pieces of code that does one thing (which is easy
-- to reason about) and then composing than you get a lot of power.
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = encode (-n) xs

-- letter frequency from a english text
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1,  6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: [Char] -> Int
lowers xs = sum [1 | x <- xs, isLower x]

count :: Char -> [Char] -> Int
count c xs = sum [1 | x <- xs, x == c]

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
        where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x' == x]

crack xs = decode factor xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

main = do
    print $ map let2int ['a'..'z']
    print $ map int2let [0..25]
    print $ map (shift (-3)) ['a'..'z']
    print $ decode 3 $ encode 3 "haskell is fun"
    print $ crack (encode 5 "hello world this is fun")
