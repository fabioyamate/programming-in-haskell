-- The full code is implemented in parser.hs
-- this is only the answer

-- (a) Grammar rules:
--   expr ::= expr - nat | nat
--   nat  ::= 0 | 1 | 2 | ...

-- (b)
expr :: Parser Int
expr = (expr >>= \e -> symbol "-" >> nat >>= \n -> return (e - n))
       +++ nat

-- (c) However, there is a problem with this expression, because it
-- is a recursive function that never ends since it never reaches the
-- symbol "-".

-- (d) To fix this recursive approach, we first parse all natural number
-- in subtraction expression and then reduce from left with the (-) function
-- where the initial state is the first natural number parsed.

expr :: Parser Int
expr = nat >>= \x ->
    many (symbol "-" >> nat) >>= \xs -> return (foldl (-) x xs)
