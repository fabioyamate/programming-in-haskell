-- 1. precedence of infix `div` over `lenght xs`, because it could consider as div with
--    3 args. `div a length xs`
-- 2. function names must be in lower case, upper case are for types
-- 3. indentation of `xs` expression

n = a `div` (length xs)
    where
        a = 10
        xs = [1,2,3,4,5]

-- main = do
--     putStrLn . show $ n
