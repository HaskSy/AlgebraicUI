{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Lib.Effects where

-- import Data.OpenUnion

import Control.Concurrent (yield)
import Control.Monad (ap, when)
import Control.SimpleMonad
import Data.Bool (bool)
import Data.Functor.Identity
import Debug.Trace (traceId, traceShow, traceShowM)
import GHC.Base (undefined)
import Utils

data Free f a = Pure a | Impure (f (Free f a)) deriving (Functor)

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

deriving instance (Eq a, Eq (f (Free f a))) => Eq (Free f a)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance (Functor f) => Monad (Free f) where
  Pure a >>= f = f a
  Impure a >>= f = Impure $ (>>= f) <$> a

-- | Тип монады, в которой будут доступны все эффекты из effs.
type Eff effs a = Free effs a

data (eff :+: effs) a = L (eff a) | R (effs a) deriving (Functor)

-- Scope???

data Lift eff a where
  Lift :: (eff a) -> Lift eff a
  deriving (Functor)

type EffLifted effs a = Free (Lift effs) a

-- liftEff :: (Functor effs) => Eff effs a -> EffLifted effs a
-- liftEff = \case
--     Pure x -> Pure x

liftHigherEff :: (Functor effs1, Functor effs2) => EffLifted effs1 a -> EffLifted (effs1 :+: effs2) a
liftHigherEff (Pure x) = Pure x
liftHigherEff (Impure (Lift eff)) = Impure $ Lift $ L $ liftHigherEff <$> eff
-- Is it scope???

-- "syntah highlighting line"
liftF :: (Functor effs) => Eff effs a -> Eff (eff :+: effs) a
liftF = \case
  Pure x -> Pure x
  Impure effs -> Impure $ R $ liftF <$> effs

-- Empty Effect
type EmptyEff = Identity

runEmptyEff :: Eff EmptyEff a -> a
runEmptyEff = \case
  Pure a -> a
  Impure nothing -> runIdentity $ runEmptyEff <$> nothing

-- <Empty Effect>

-- State Effect
data StateEff s comp
  = Put s (() -> comp)
  | Get () (s -> comp)
  deriving (Functor)

runStateEff :: (Functor effs) => Eff (StateEff s :+: effs) a -> s -> Eff effs (a, s)
runStateEff comp st = case comp of
  Pure x -> Pure (x, st)
  Impure (L op) -> case op of
    Get () cont -> runStateEff (cont st) st
    Put st' cont -> runStateEff (cont ()) st'
  Impure (R effs) -> Impure $ (`runStateEff` st) <$> effs

getEff :: Eff (StateEff s :+: effs) s
getEff = Impure $ L $ Get () Pure

putEff :: s -> Eff (StateEff s :+: effs) ()
putEff s = Impure $ L $ Put s Pure

-- <State Effect>

-- Exception Effect
newtype ExceptionEff e comp = Throw e
  deriving
    ( -- | Бросание исключения - продолжения нет, так как в случае
      -- исключения оно отбрасывается.
      Functor
    )

runExceptionEff :: (Functor effs) => Eff (ExceptionEff e :+: effs) a -> Eff effs (Either e a)
runExceptionEff = \case
  Pure x -> pure $ Right x
  Impure (L (Throw e)) -> pure $ Left e
  Impure (R effs) -> Impure $ runExceptionEff <$> effs

throwEff :: e -> Eff (ExceptionEff e :+: effs) a
throwEff e = Impure $ L $ Throw e

-- <Exception Effect>

-- Logger Effect
data LoggerEff e comp = Log e (() -> comp) deriving (Functor)

runLoggerEff :: (Show e, Functor effs) => Eff (LoggerEff e :+: effs) a -> Eff effs ([e], a)
runLoggerEff = \case
  Pure x -> Pure ([], x)
  Impure (L (Log msg comp)) -> do
    (msgs, result) <- runLoggerEff (comp ())
    Pure (msg : msgs, result)
  Impure (R effs) -> Impure $ runLoggerEff <$> effs

logEff :: e -> Eff (LoggerEff e :+: effs) ()
logEff msg = Impure $ L $ Log msg Pure

-- runLoggerEffOU :: (Show e) => Eff (Union (LoggerEff e ': effs)) a -> Eff (Union effs) ([e], a)
-- runLoggerEffOU = \case
--     Pure x -> Pure ([], x)
--     Impure (t :: Union (LoggerEff e ': effs) a) -> case decomp t of
--         Left t -> case t of {}
--         Right (Log msg comp) -> do
--             let (msgs, result) = runLoggerEffOU (comp ())
--             Pure (msg:msgs, result)
--
-- <Logger Effect>

-- Generator Effect
data GeneratorEff a comp = Yield a (() -> comp) deriving (Functor)

runGeneratorEff :: (Functor effs) => Eff (GeneratorEff a :+: effs) () -> Eff effs [a]
runGeneratorEff = \case
  Pure () -> pure []
  Impure (L (Yield x comp)) -> do
    xs <- runGeneratorEff (comp ())
    Pure (x : xs)
  Impure (R effs) -> Impure $ runGeneratorEff <$> effs

yieldEff :: a -> Eff (GeneratorEff a :+: effs) ()
yieldEff x = Impure $ L $ Yield x Pure

-- <Generator Effect>

data UITree a = UILeaf a | UISingleNode a (UITree a) | UINode a [UITree a] | UIEmpty

instance (Monoid m) => Semigroup (UITree m) where
  (<>) UIEmpty bot = bot
  (<>) top UIEmpty = top
  (<>) a b = case a of
    UILeaf _ -> case b of
      UINode s' t -> UINode s' (a : t)
      x -> UINode mempty [a, x]
    _ -> UINode mempty [a, b]

instance (Monoid m) => Monoid (UITree m) where
  mempty = UIEmpty

prettyPrint :: (Show m) => Int -> UITree m -> String
prettyPrint level = \case
  UIEmpty -> indent level ++ "UIEmpty\n"
  (UILeaf mod) -> indent level ++ "UILeaf " ++ show mod ++ "\n"
  (UINode mod children) ->
    indent level
      ++ "UINode "
      ++ show mod
      ++ " [\n"
      ++ concatMap (prettyPrint (level + 1)) children
      ++ indent level
      ++ "]\n"
  (UISingleNode mod child) ->
    indent level
      ++ "UISingleNode "
      ++ show mod
      ++ " (\n"
      ++ prettyPrint (level + 1) child
      ++ indent level
      ++ ")\n"
  where
    indent :: Int -> String
    indent n = replicate (n * 4) ' '

instance (Show m) => Show (UITree m) where
  show = prettyPrint 0

data Modifiers where
  Modifiers :: {color :: String} -> Modifiers
  deriving (Show)

-- newtype BreakEff comp = Break comp deriving (Functor)
--
-- runBreakEff :: (Functor effs) => Eff (BreakEff :+: effs) m -> Eff EmptyEff (Eff effs m)
-- runBreakEff = \case
--     Pure _ -> pure mempty
--     Impure (L (Break t)) -> pure t
--     Impure (R effs) -> Impure $ runBreakEff <$> effs
--
-- breakEff :: c -> Eff (BreakEff :+: effs) c
-- breakEff cont = Impure $ L (Break cont)

-- data ForgetEff comp where
--   Forget :: ForgetEff comp
--   deriving (Functor)
--
-- runForgetEff :: (Functor effs) => Eff (ForgetEff :+: effs) m -> Eff effs ()
-- runForgetEff = \case
--     Pure _ -> pure ()
--     Impure (L Forget) -> pure ()
--     Impure (R effs) -> Impure $ runForgetEff <$> effs
--
-- forgetEff :: Eff (ForgetEff :+: effs) ()
-- forgetEff = Impure $ L Forget

-- UI Effects

-- m - modifiers
data ComponentEff m comp
  = CLeaf m comp
  | CNode m comp comp
  | CSingleNode m comp comp
  deriving (Functor)

runComponentEff ::
  (Show m, Monoid m, Functor effs) =>
  Eff (ComponentEff m :+: effs) () ->
  Eff effs (UITree m)
runComponentEff = \case
  Pure () -> pure mempty
  Impure (L t) -> case t of
    CLeaf a cont -> do
      res <- runComponentEff cont
      pure $ UILeaf a <> res
    CNode a subcont cont -> do
      undefined
    CSingleNode a subcont cont -> do
      res <- runComponentEff subcont
      -- traceShowM res
      res' <- runComponentEff cont
      pure $ UISingleNode a res <> res'
  Impure (R effs) -> Impure $ runComponentEff <$> effs

leafEff :: m -> Eff (ComponentEff m :+: effs) ()
leafEff x = Impure $ L $ CLeaf x (Pure ())

nestedEff ::
  m ->
  Eff (ComponentEff m :+: effs) () ->
  Eff (ComponentEff m :+: effs) ()
nestedEff m eff = Impure $ L $ CSingleNode m eff (Pure ())

leafEff' ::
  m ->
  Eff (GeneratorEff (UITree m) :+: effs) ()
leafEff' m = yieldEff $ UILeaf m

nestedEff' ::
  (Monoid m) =>
  m ->
  Eff (GeneratorEff (UITree m) :+: EmptyEff) () ->
  Eff (GeneratorEff (UITree m) :+: effs) ()
nestedEff' m eff = do
  let subeff = runEmptyEff $ runGeneratorEff eff
  if length subeff == 1
    then yieldEff $ UISingleNode m (head subeff)
    else yieldEff $ UISingleNode m (UINode mempty subeff)

