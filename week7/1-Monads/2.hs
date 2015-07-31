--(>>=) :: m a -> (a -> m b) -> m b

join :: Monad m => m (m a) -> m a
join x = x >>= id
