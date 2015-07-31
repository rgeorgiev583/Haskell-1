--(>>=) :: m a -> (a -> m b) -> m b

join :: Monad m => m (m a) -> m a
join x = x >>= id

sequenceM :: Monad m => [m a] -> m [a]
--(>>=) :: m a -> (a -> m [a]) -> m [a]
--sequenceM l = foldl (\a x -> a >>= (\y -> return y +)) l
sequenceM (x : xs) = x >>= \y -> sequenceM xs >>= \l -> return (y : l)
sequenceM    _     = return [] 
