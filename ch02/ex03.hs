-- 1. backticks instead of accent
-- 2. function names must be in lower case, upper case are for types
-- 3. indentation of `xs` expression

n = a `div` (length xs)
    where
        a = 10
        xs = [1,2,3,4,5]

-- main = do
--     putStrLn . show $ n
