import Control.Monad

newtype Writer w a = Writer { runWriter :: (a, w) }

execWriter :: Writer w a -> w
execWriter = snd . runWriter

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = Writer . f . runWriter

tell :: w -> Writer w ()
tell x = Writer ((), x)

instance Monoid w => Functor (Writer w) where
    fmap = liftM

instance Monoid w => Applicative (Writer w) where
    pure  = return
    (<*>) = ap

instance Monoid w => Monad (Writer w) where
    --return :: (a, w) -> Writer w a
    return x = Writer (x, mempty)

    --(>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    writer >>= f = f $ fst $ runWriter writer
