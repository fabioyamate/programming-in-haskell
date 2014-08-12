-- This is the entire parse file from the book.
--
-- I couldn't use the do-notation to run the code as expected. For some reason
-- the do-notation wasn't consuming the String.
--
import Prelude hiding (return, (>>=), (>>))
import Data.Char

-- this means that we have a type that is a function that receives a String and returns
-- [(parsed, rest)]
type Parser a = String -> [(a, String)]

-- return is a function that receives a value and wraps it into a Parser
return :: a -> Parser a
return v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

-- Parser Char = String -> [(Char, String)]
item :: Parser Char
item = \inp -> case inp of
                [] -> []
                (x:xs) -> [(x, xs)]

-- parse is a function that applies the Parser function to the String input
-- It could be simplified as
--
--   parse p = p
--
-- which means that parse is the identity function. However, it is common to
-- have a function like this.
parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

-- combining parsers in sequence
-- p1 >>= \inp -> p2
--
-- f is a function that receives the result of parsing p1 with the input
-- and returns a Parser b
--
-- It is harder to understand what happens behind the scenes. Lets check this
-- parser construction:
--
--   parser1 >>= \v -> parser2 >>= \u -> return (v,u)
--
-- Lets put some parens
--
-- parser1 >>= (\v -> parser2 >>= \u -> return (v,u))
--
-- Then, lets replace the right expression with as a function `f`
--
--   parser1 >>= f
--
-- Note that the (>>=) returns a function that receives the String `inp` as
-- argument.
--
-- The `inp` is applied to the `parser1`:
--
--   parser1 >>= f $ inp
--
-- The application gives the `inp` to `parser1`, once it applies it
-- returns a type `[(a, String)]`, this is then pattern matched, and
-- the matched value is passed to the function `f`.
--
-- The application `f v` returns a Parser that is passed to `parse` in
-- sequence with the left String.
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
                    [] -> []
                    [(v, out)] -> parse (f v) out

(>>) :: Parser a -> Parser b -> Parser b
p >> q = p >>= \_ -> q

p :: Parser (Char, Char)
p = item >>= \x -> item >> item >>= \y -> return (x, y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    [] -> parse q inp
                    [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then return x
                       else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = char x >> string xs >> return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v -> many p >>= \vs -> return (v:vs)

ident :: Parser String
ident = lower >>= \x -> many alphanum >>= \xs -> return (x:xs)

nat :: Parser Int
nat = many1 digit >>= \xs -> return (read xs)

int :: Parser Int
int = (symbol "-" >> nat >>= \n -> return (-n)) +++ nat

space :: Parser ()
space = many (sat isSpace) >> return ()

token :: Parser a -> Parser a
token p = space >> p >>= \v -> space >> return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

comment :: Parser ()
comment = symbol "--" >> many (sat (/= '\n')) >> char '\n' >> return ()

pp :: Parser [Int]
pp = symbol "[" >> natural >>= \n -> many (symbol "," >> natural) >>= \ns -> symbol "]" >> return (n:ns)

expr :: Parser Int

expr = term >>= \t ->
    (symbol "+" >> expr >>= \e -> return (t + e))
    +++ (symbol "-" >> expr >>= \e -> return (t - e))
    +++ return t

term = power >>= \p ->
    (symbol "*" >> term >>= \t -> return (p * t))
    +++ (symbol "/" >> term >>= \t -> return (p `div` t))
    +++ return p

power = factor >>= \f ->
    (symbol "^" >> power >>= \p -> return (f ^ p))
    +++ return f

factor = (symbol "(" >> expr >>= \e -> symbol ")" >> return e) +++ natural

eval xs = case parse expr xs of
            [(n, [])] -> n
            [(_, out)] -> error ("unused input " ++ out)
            [] -> error "invalid input"

negExpr :: Parser Int
negExpr = nat >>= \x ->
    many (symbol "-" >> nat) >>= \xs -> return (foldl (-) x xs)

negEval xs = case parse negExpr xs of
            [(n, [])] -> n
            [(_, out)] -> error ("unused input " ++ out)
            [] -> error "invalid input"

main = do
    print $ parse (return 1) "abc"
    print $ parse item "abc"
    print $ parse p "abcdf"
    print $ parse (string "abc") "abcdef"
    print $ parse digit "1234"
    print $ parse int  "1"
    print $ parse space  " kjlm "
    print $ parse identifier  " kjlm "
    print $ parse pp " [1, 2, 3] "
    print $ eval "4-2+4" == -2
    print $ eval "2*3+4" == 10
    print $ eval "24/3*4" == 2
    print $ eval "6/3-4" == -2
    print $ eval "2^(3*4)" == 4096
    print $ parse comment "-- hello world\nabc"
    print $ eval "2" == 2
    print $ negEval "1-2-3-4-5" == -13
    print $ eval "-1"
