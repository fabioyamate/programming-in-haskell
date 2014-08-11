-- The full code is implemented in parser.hs
-- this is only the answer

comment :: Parser ()
comment = symbol "--" >> many (sat (/= '\n')) >> char '\n' >> return ()
