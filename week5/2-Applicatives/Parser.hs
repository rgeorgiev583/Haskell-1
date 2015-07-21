module Parser where


import Control.Applicative

newtype Parser a =
  Parser { parse :: String -> Maybe (a, String) }

first :: Parser Char
first = Parser f
  where f :: String -> Maybe (Char, String)
        f (x : xs) = Just (x, xs)
        f    _     = Nothing

instance Functor Parser where
  fmap f (Parser p) = undefined

instance Applicative Parser where
  pure  = undefined
  (<*>) = undefined

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser g
  where g :: String -> Maybe (Char, String)
        g (x : xs) = if f x then Just (x, xs) else Nothing
        g    _     = Nothing

char :: Char -> Parser Char
char = undefined

openingBrace :: Parser Char
openingBrace = undefined

closingBrace :: Parser Char
closingBrace = undefined

inBraces :: Parser a -> Parser a
inBraces p = openingBrace *> p <* closingBrace

instance Alternative Parser where
  empty = undefined
  (<|>) = undefined

oneOrMore, zeroOrMore  :: Parser a -> Parser [a]
oneOrMore = undefined

zeroOrMore = undefined
