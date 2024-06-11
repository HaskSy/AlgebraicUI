{-# HLINT ignore "Use tuple-section" #-}

module Control.SimpleMonad where

import Control.Monad (ap)

-- -----------------------------------------------------------------------------

newtype Reader e a = Reader { runReader :: e -> a }
  deriving Functor

instance Applicative (Reader e) where
  pure a = Reader $ const a
  (<*>) = ap

instance Monad (Reader e) where
  ma >>= g = Reader $ \e ->
    let a = runReader ma e in
    runReader (g a) e

reader :: (e -> a) -> Reader e a
reader = Reader

ask :: Reader e e
ask = asks id -- Reader id

-- Встраивает другое вычисление, подменяя для него окружение.
local :: (e -> e') -> Reader e' a -> Reader e a
local f m = Reader $ \e ->
  let e' = f e in
  let g = runReader m in
  g e'

asks :: (e -> a) -> Reader e a
asks = reader

-- -----------------------------------------------------------------------------

newtype Writer w a = Writer { runWriter :: (a, w) }
  deriving (Show, Eq, Ord, Functor)

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  (<*>) = ap

instance Monoid w => Monad (Writer w) where
  ma >>= g =
    let (a, wa) = runWriter ma in
    let (b, wb) = runWriter (g a) in
    Writer (b, wa <> wb)

execWriter :: Writer w a -> w
execWriter = snd . runWriter

writer :: (a, w) -> Writer w a
writer = Writer

tell :: w -> Writer w ()
tell w = writer ((), w)

listen :: Writer w a -> Writer w (a, w)
listen m = writer $ let (a, w) = runWriter m in ((a, w), w) -- listens id

listens :: (w -> b) -> Writer w a -> Writer w (a, b)
listens f m = writer $ let (a, w) = runWriter m in ((a, f w), w)

censor :: (w -> w') -> Writer w a -> Writer w' a
censor f m = writer $ let (a, w) = runWriter m in (a, f w)

-- -----------------------------------------------------------------------------

newtype State s a = State { runState :: s -> (a, s) }
  deriving (Functor)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) = ap

instance Monad (State s) where
  fa >>= g = State $ \s ->
    let (a, s') = runState fa s in
    runState (g a) s'

state :: (s -> (a, s)) -> State s a
state = State

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

get :: State s s
get = state $ \s -> (s, s) -- gets id

put :: s -> State s ()
put s = state $ const ((), s)

gets :: (s -> a) -> State s a
gets f = state $ \s -> (f s, s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = state $ f . runState m

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)

modify' :: (s -> s) -> State s ()
modify' f = do
  s <- get
  put $! f s

withState :: (s -> s) -> State s a -> State s a
withState f m = modify f *> m
