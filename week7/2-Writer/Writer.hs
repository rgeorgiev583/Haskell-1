newtype Writer w a = Writer { runWriter :: (a, w) }

execWriter :: Writer w a -> w
execWriter = snd . runWriter

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f writer = Writer { runWriter = result }
    where result = f (runWriter writer)

tell :: w -> Writer w ()
tell = Writer { runWriter = ((), w) }
