import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- step 1: add function to include the parity bit
parity :: [Bit] -> [Bit]
parity bits = bits ++ [parityBit]
    where ones = length (filter (==1) bits)
          parityBit = ones `mod` 2

-- step 2: include the parity when encoding
encode :: String -> [Bit]
encode = concat . map (parity . make8 . int2bin . ord)

-- step 3: change chop8 to chop9 to include the parity bit
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

-- step 4: add function to check parity bit
parityCheck :: [Bit] -> [Bit]
parityCheck bits
        | parity binNumber == bits = binNumber
        | otherwise = error "invalid parity"
        where binNumber = take 8 bits

-- step 5: include parity check when decoding
decode :: [Bit] -> String
decode = map (chr . bin2int . parityCheck) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode

main = do
    print $ transmit "higher-order functions are easy"
    print $ faultyTransmit "higher-order functions are easy"
