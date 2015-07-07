data IntList = Empty | Cons Int IntList
    deriving (Show)

fromList :: [Int] -> IntList
fromList (x : xs) = Cons x (fromList xs)
fromList    _     = Empty

toList :: IntList -> [Int]
toList (Cons x xs) = x : toList xs
toList    Empty    = []

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList f l = fromList (filter f (toList l))

foldlIntList :: (a -> Int -> a) -> a -> IntList -> a
foldlIntList f s l = foldl f s (toList l)

mapIntList :: (Int -> a) -> IntList -> [a]
mapIntList f l = map f (toList l)
