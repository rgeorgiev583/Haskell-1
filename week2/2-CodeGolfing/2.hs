map' :: (a -> b) -> [a] -> [b]
map' f l = foldl (\a x -> a ++ [f x]) [] l

filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = foldl (\a x -> if f x then a ++ [x] else a) [] l

quicksort :: Ord a => [a] -> [a]
quicksort (x : xs) = quicksort l ++ [x] ++ quicksort r
    where
        l = less    x xs
        r = greater x xs
        less    n l = filter' (\x -> x < n) l
        greater n l = filter' (\x -> x > n) l
quicksort    _     = []

repeat' :: a -> [a]
repeat' x = x : repeat' x

cycle' :: [a] -> [a]
cycle' (x : xs) = x : cycle' (xs ++ [x])

--reduce :: (a -> a -> a) -> a -> [a] -> a
--reduce f s (x : xs) = reduce f (f s x) xs
--reduce f s    _     = s

--TODO: Fix it: make it work.
cycle'' :: [a] -> [a]
cycle'' l = foldl (++) [] (repeat' l)

--TODO: Implement it using foldl.
every :: Int -> [a] -> [a]
every n l = helper n 0 l
    where
        helper 0 m (x : xs) = []
        helper 1 m (x : xs) = x : helper (m + 1) 0       xs
        helper n m (x : xs) =     helper (n - 1) (m + 1) xs
        helper _ _    _     = []

--TODO: Implement it using foldl.
localMaxima :: Ord a => [a] -> [a]
localMaxima (x : y : z : xs)
    | x <= y && y >= z = y : localMaxima (y : z : xs)
    | otherwise        =     localMaxima (y : z : xs)
localMaxima        _ = []

mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap f l = map' (\x -> map' f x) l

filterFilter :: (a -> Bool) -> [[a]] -> [[a]]
filterFilter f l = map' (\x -> filter' f x) l

unit :: Int -> Int -> [[Int]]
unit x n = map' helper [0 .. n - 1]
    where
        helper m = take m (repeat' 0) ++ [x] ++ take (n - m - 1) (repeat' 0)

row :: Int -> [[a]] -> [a]
row 0 (r :  _) = r
row n (_ : rs) = row (n - 1) rs
row _    _     = error "Cannot get a row from an empty matrix!"

row' :: Int -> [[a]] -> [a]
row' _ []     = error "Cannot get a row from an empty matrix!"
row' n matrix = head (foldl (\a x -> tail a) matrix [0 .. n - 1])

--TODO: Implement it using foldl.
transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' matrix
    | not (null (head matrix)) = map' head matrix : transpose' (map' tail matrix)
    | otherwise                = []

sumMatrices' :: Num a => [[a]] -> [[a]] -> [[a]]
sumMatrices' (x : xs) (y : ys) = zipWith (+) x y : sumMatrices' xs ys
sumMatrices'    _        _     = []

--zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
--zipWith' f    _        _     = []

zipZipWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipZipWith f ll lr = zipWith (\x y -> zipWith f x y) ll lr

--zipZip :: [[a]] -> [[b]] -> [[(a, b)]]
--zipZip m1 m2 = zipZipWith (\x y -> (x, y)) m1 m2

sumMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
sumMatrices m1 m2 = zipZipWith (+) m1 m2

col :: Int -> [[a]] -> [a]
col _ []     = error "Cannot get a column from an empty matrix!"
col 0 matrix = map' head matrix
col n matrix = col (n - 1) (map' tail matrix)

col' :: Int -> [[a]] -> [a]
col' _ []     = error "Cannot get a column from an empty matrix!"
col' n matrix = map' head (foldl (\a x -> map' tail a) matrix [0 .. n - 1])

multMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
multMatrices m1 m2 = mapMap (\x -> (foldl (+) 0 (zipWith (*) (row (fst x) m1) (col (snd x) m2)))) [[(x, y) | y <- [0 .. length m1 - 1]] | x <- [0 .. length (head m2) - 1]]

--TODO: Histogram (couldn't understand it!)
