--(>>=) :: m a -> (a -> m b) -> m b

join :: Monad m => m (m a) -> m a
join x = x >>= id

sequenceM :: Monad m => [m a] -> m [a]
--(>>=) :: m a -> (a -> m [a]) -> m [a]
--sequenceM l = foldl (\a x -> a >>= (\y -> return y +)) l
sequenceM (x : xs) = x >>= \y -> sequenceM xs >>= \l -> return (y : l)
sequenceM    _     = return [] 

replicateM :: Monad m => Int -> m a -> m [a]
replicateM 0 x = return []
replicateM n x = x >>= \y -> replicateM (n - 1) x >>= \l -> return (y : l)

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f (x : xs) = f x >>= \y -> filterM f xs >>= \l -> if y then return (x : l) else return l
filterM f    _     = return []

mapMM :: Monad m => (a -> m b) -> [a] -> m [b]
mapMM f (x : xs) = f x >>= \y -> mapMM f xs >>= \l -> return (y : l)
mapMM f    _     = return []
