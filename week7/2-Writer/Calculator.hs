module Calculator where

data Oper  = Plus | Minus | Times | Divide | Mod
data Token = Val Integer | Op Oper

parseToken :: String -> Token
parseToken "+" = Op Plus
parseToken "-" = Op Minus
parseToken "*" = Op Times
parseToken "/" = Op Divide
parseToken "%" = Op Mod
parseToken  s  = Val (read s :: Integer)

calculateRPN :: [Integer] -> [Token] -> Integer
calculateRPN [result] [] = result
calculateRPN stack (Val x : ts) = calculateRPN (x : stack) ts
calculateRPN (ropnd : lopnd : stack) (Op op : ts) = case op of
                                                      Plus   -> calculateRPN (lopnd   +   ropnd : stack) ts
                                                      Minus  -> calculateRPN (lopnd   -   ropnd : stack) ts
                                                      Times  -> calculateRPN (lopnd   *   ropnd : stack) ts
                                                      Divide -> calculateRPN (lopnd `div` ropnd : stack) ts
                                                      Mod    -> calculateRPN (lopnd `mod` ropnd : stack) ts
calculateRPN _ _ = error "wrong RPN syntax"

calculate :: String -> Integer
calculate s = calculateRPN [] (map parseToken $ words s)
