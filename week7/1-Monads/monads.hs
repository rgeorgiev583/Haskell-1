import List
import Control.Monad

data Maybe' a = Nothing' | Just' a
--data (->) a = 

instance Functor Maybe' where
  fmap = liftM

instance Applicative Maybe' where
  pure  = return
  (<*>) = ap

instance Monad Maybe' where
  --return :: a -> m a
  return x = Just' x

  --(>>=) :: m a -> (a -> m b) -> m b
  Nothing' >>= _ = Nothing'
  Just' x  >>= f = f x

instance Functor List where
  fmap = liftM

instance Applicative List where
  pure  = return
  (<*>) = ap

instance Monad List where
  --return :: a -> List a
  return x = Cons x Empty

  --(>>=) :: List a -> (a -> List b) -> List b
  Empty     >>= _ = Empty
  Cons x xs >>= f = append (f x) (foldlList (\a x -> append a x) Empty (mapList f xs))
