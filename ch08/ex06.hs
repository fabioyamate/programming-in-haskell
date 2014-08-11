-- The full code is implemented in parser.hs
-- this is only the answer

expr :: Parser Int
expr = term >>= \t ->
    (symbol "+" >> expr >>= \e -> return (t + e))
    +++ (symbol "-" >> expr >>= \e -> return (t - e))
    +++ return t

term :: Parser Int
term = factor >>= \f ->
    (symbol "*" >> term >>= \t -> return (f * t))
    +++ (symbol "/" >> term >>= \t -> return (f `div` t))
    +++ return f
