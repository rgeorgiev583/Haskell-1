data IntList = Empty | Cons Int IntList
    deriving (Show)

fromList :: [Int] -> IntList
fromList (x : xs) = Cons x (fromList xs)
fromList    _     = Empty

toList :: IntList -> [Int]
toList (Cons x xs) = x : toList xs
toList    Empty    = []
