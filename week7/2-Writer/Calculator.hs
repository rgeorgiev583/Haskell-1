module Calculator where

import Parser
import qualified Data.Char as C (isDigit)

integer :: Parser Integer
integer = read <$> (oneOrMore $ satisfy C.isDigit)

--calculate :: String -> Integer

