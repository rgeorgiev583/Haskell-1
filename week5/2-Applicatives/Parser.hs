module Parser where


import Control.Applicative
import qualified Control.Arrow as A (first)


newtype Parser a =
  Parser { parse :: String -> Maybe (a, String) }

first :: Parser Char
first = Parser f
  where f :: String -> Maybe (Char, String)
        f (x : xs) = Just (x, xs)
        f    _     = Nothing

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s -> A.first f <$> p s

instance Applicative Parser where
  --pure :: a -> Parser a
  pure v = Parser $ \s -> Just (v, s)

  --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser p1 <*> Parser p2 = Parser $ \s -> case p1 s of
                                             Just (f, s') -> fmap (A.first f) (p2 s')
                                             Nothing      -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser g
  where g :: String -> Maybe (Char, String)
        g (x : xs) = if f x then Just (x, xs) else Nothing
        g    _     = Nothing

char :: Char -> Parser Char
char = satisfy . (==)

openingBrace :: Parser Char
openingBrace = satisfy (== '(')

closingBrace :: Parser Char
closingBrace = satisfy (== ')')

inBraces :: Parser a -> Parser a
inBraces p = openingBrace *> p <* closingBrace


instance Alternative Parser where
  --empty :: Parser a
  empty = Parser $ const Nothing

  --(<|>) :: Parser a -> Parser a -> Parser a
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

oneOrMore, zeroOrMore  :: Parser a -> Parser [a]
oneOrMore = undefined

zeroOrMore = undefined
