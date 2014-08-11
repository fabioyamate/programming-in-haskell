-- The full code is implemented in parser.hs
-- this is only the answer

-- Grammar rules:
--   expr   ::= term (+ expr | empty)
--   term   ::= power (* term | empty)
--   power  ::= factor (^ power | empty)
--   factor ::= (expr) | nat
--   nat    ::= 0 | 1 | 2 | ...

expr :: Parser Int
expr = term >>= \t ->
    (symbol "+" >> expr >>= \e -> return (t + e))
    +++ return t

term = power >>= \p ->
    (symbol "*" >> term >>= \t -> return (p * t))
    +++ return p

power = factor >>= \f ->
    (symbol "^" >> power >>= \p -> return (f ^ p))
    +++ return f

factor = (symbol "(" >> expr >>= \e -> symbol ")" >> return e) +++ natural
