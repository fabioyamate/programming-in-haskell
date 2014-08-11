-- The full code is implemented in parser.hs
-- this is only the answer

int :: Parser Int
int = (symbol "-" >> nat >>= \n -> return (-n)) +++ nat
