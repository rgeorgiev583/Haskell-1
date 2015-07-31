import List

data Maybe' a = Nothing' | Just' a
--data (->) a = 

instance Monad Maybe' where
  --return :: a -> m a
  return x = Just' x

  --(>>=) :: m a -> (a -> m b) -> m b
  Nothing' >>= _ = Nothing'
  Just' x  >>= f = f x

instance Monad List where
  --return :: a -> List a
  return x = Cons x Empty

  --(>>=) :: List a -> (a -> List b) -> List b
  Empty     >>= _ = Empty
  Cons x xs >>= f = append (f x) (foldlList (\a x -> append a x) Empty (mapList f xs))
