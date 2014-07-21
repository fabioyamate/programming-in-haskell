-- Caesar cipher

module CaesarCipher where
import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

-- change 1: shift on upper letters
shift :: Int -> Char -> Char
shift n c | isUpper c = toUpper $ shift n $ toLower c
          | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = encode (-n) xs

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1,  6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- change 2: lowers to letters
letters :: [Char] -> Int
letters xs = sum [1 | x <- xs, isLetter x]

count :: Char -> [Char] -> Int
count c xs = sum [1 | x <- xs, x == c]

-- change 3: use letters instead of lowers
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
        where n = letters xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x' == x]

crack xs = decode factor xs
    where
        factor = head $ positions (minimum chitab) chitab
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

main = do
    print $ encode 3 "Haskell is Fun"
    print $ crack (encode 5 "Hello World! Haskell is SUPER Fun!!! Yeah...")
