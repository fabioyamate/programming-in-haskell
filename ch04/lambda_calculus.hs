-- Lambda Calculus References
-- Reference http://www.flyingmachinestudios.com/programming/a-taste-of-the-lambda-calculus/

-- λx.x
id' = \x -> x

-- λfirst.λsecond.first
const' = \x -> \y -> x

-- λfn.λarg.(fn arg)
-- (function application)
apply' = \f -> \a -> f a

-- Function application
-- (λx.x 1)
--
-- In Haskell we can use () to call a function, but they are
-- not necessary, unless precedence application is required.

-- self application combinator
-- λs.(s s)
--
-- \s -> s s
--
-- This expression is not possible in a typed language.

-- λfirst.λsecond.λfunc.((func first) second)
pair' :: a -> a -> (a -> a -> a) -> a
pair' = \x -> \y -> \f -> ((f x) y)

-- λfirst.λsecond.first
fst' :: a -> a -> a
fst' = \x -> \y -> x -- ~ const'

-- λfirst.λsecond.second
snd' :: a -> a -> a
snd' = \x -> \y -> y

-- This pair lambda expression can be represented as a conditional
cond' = pair'
true' = fst'
false' = snd'

not' = \x -> ((x false') true')
and' = \x -> \y -> ((x y) false')
or' = \x -> \y -> ((x true') y)

main = do
    putStrLn . show $ id' 1
    putStrLn . show $ (const' 1) 2
    putStrLn . show $ apply' (1+) 2
    putStrLn . show $ p fst'
    putStrLn . show $ p snd'
    putStrLn . show $ cond' True False true'
    putStrLn . show $ cond' True False false'
    putStrLn "not'"
    putStrLn . show $ cond' True False (not' true')
    putStrLn . show $ cond' True False (not' false')
    putStrLn "and'"
    putStrLn . show $ cond' True False (and' true' false')
    putStrLn . show $ cond' True False (and' true' true')
    putStrLn "or'"
    putStrLn . show $ cond' True False (or' false' true')
    putStrLn . show $ cond' True False (or' false' false')
    where p = pair' 1 2
