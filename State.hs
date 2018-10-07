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
  fmap f s = State $ \u -> let (x, v) = runState s u in (f x, v)

instance Applicative (State s) where
  pure x = State $ \u -> (x, u)
  t <*> s = State $ \u -> let (x, v) = runState s u; (f, w) = runState t v in (f x, w)
 
instance Monad (State s) where
  s >>= f = State $ \u -> let (x, v) = runState s u in runState (f x) v

evalState :: State s a -> s -> a
evalState s = fst . runState s

execState :: State s a -> s -> s
execState s = snd . runState s

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f s = State $ f . runState s

withState :: (s -> s) -> State s a -> State s a
withState f s = State $ \u -> let (x, v) = runState s u in (x, f v)

get :: State s s
get = State $ \u -> (u, u)

put :: s -> State s ()
put u = State $ \u -> ((), u)

modify :: (s -> s) -> State s ()
modify f = State $ \u -> ((), f u)

modify' :: (s -> s) -> State s ()
modify' f = State $ \u -> f u `seq` ((), f u)

gets :: (s -> a) -> State s a
gets f = State $ \u -> (f u, u)
