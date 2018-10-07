module State
  ( State
  , runState
  , evalState
  , execState
  , mapState
  , withState
  , get
  , put
  , modify
  , modify'
  , gets
  ) where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f s = State h
    where
      g u = runState s u
      h u = (f x, v)
        where
          (x, v) = g u

instance Applicative (State s) where
  pure x = State g
    where
      g u = (x, u)

  t <*> s = State k
    where
      g u = runState s u
      h u = runState t u
      k u = (f x, w)
        where
          (x, v) = g u
          (f, w) = h v
 
instance Monad (State s) where
  s >>= f = State h
    where
      g u = runState s u
      h u = (y, w)
        where
          (x, v) = g u
          (y, w) = runState (f x) v

evalState :: State s a -> s -> a
evalState s u = x
  where
    (x, _) = runState s u

execState :: State s a -> s -> s
execState s u = v
  where
    (_, v) = runState s u

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f s = State g
  where
    g u = f (x, v)
      where
        (x, v) = runState s u
        (y, w) = f (x, v)

withState :: (s -> s) -> State s a -> State s a
withState f s = State g
  where
    g u = (x, f v)
      where
        (x, v) = runState s u

get :: State s s
get = State g
  where
    g u = (u, u)

put :: s -> State s ()
put u = State g
  where
    g u = ((), u)

modify :: (s -> s) -> State s ()
modify f = State g
  where
    g u = ((), f u)

modify' :: (s -> s) -> State s ()
modify' f = State g
  where
    g u = f u `seq` ((), f u)

gets :: (s -> a) -> State s a
gets f = State g
  where
    g u = (f u, u)
