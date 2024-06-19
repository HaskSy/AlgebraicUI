{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

{-# LANGUAGE StarIsType #-}

module Lib.Effects where

-- import Data.OpenUnion

import Control.Concurrent (yield)
import Control.Monad (ap, when)
import Control.SimpleMonad
import Data.Bool (bool)
import Data.Functor.Identity
import Debug.Trace (traceId, traceShow, traceShowM)
import GHC.Base (undefined, Any)
import Utils
import Foreign (IntPtr(IntPtr))
import Data.Map
import Data.Proxy
import Data.Dynamic (Dynamic (Dynamic), fromDynamic, toDyn)

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

infixr :+:
data (eff :+: effs) a = L (eff a) | R (effs a) deriving (Functor)


-- "syntax highlighting line"
liftF :: (Functor effs) => Eff effs a -> Eff (eff :+: effs) a
liftF = \case
  Pure x -> Pure x
  Impure effs -> Impure $ R $ liftF <$> effs

liftOfDespairF ::(Functor effs) => Eff effs a -> Proxy eff -> Eff (eff :+: effs) a
liftOfDespairF eff proxy = case eff of
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

runGeneratorEff :: (Functor effs) => Eff (GeneratorEff a :+: effs) x -> Eff effs [a]
runGeneratorEff = \case
  Pure _ -> pure []
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

-- Metadata for styling nodes
data Modifiers where
  Modifiers :: {color :: String} -> Modifiers
  deriving (Show)

leafEff ::
  m ->
  Eff (GeneratorEff (UITree m) :+: effs) ()
leafEff m = yieldEff $ UILeaf m

nestedEff ::
  (Monoid m) =>
  m ->
  Eff (GeneratorEff (UITree m) :+: EmptyEff) () ->
  Eff (GeneratorEff (UITree m) :+: effs) ()
nestedEff m eff = do
  let subeff = runEmptyEff $ runGeneratorEff eff
  if length subeff == 1
    then yieldEff $ UISingleNode m (head subeff)
    else yieldEff $ UISingleNode m (UINode mempty subeff)

-- After Meeting

-- IncrementEff

-- Used to give unique identifiers for buttons. 
-- Yes, in could be done using StateEff Int and even was, 
-- but I decided to make separate effect in case of switching to open union
data IncrementEff comp where
  Increment :: (Int -> comp) -> IncrementEff comp
  deriving (Functor)

increment :: Eff (IncrementEff :+: effs) Int
increment = Impure $ L $ Increment Pure

runIncrementEff :: (Functor effs) => Eff (IncrementEff :+: effs) a -> Eff effs Int
runIncrementEff = runIncrementEff' 0
  where
    runIncrementEff' :: (Functor effs) => Int -> Eff (IncrementEff :+: effs) a -> Eff effs Int
    runIncrementEff' init comp = case comp of
      Pure _ -> Pure init
      Impure (L (Increment cont)) -> runIncrementEff' (init + 1) $ cont init
      Impure (R effs) -> Impure $ runIncrementEff' init <$> effs

-- <IncrementEff>

-- EffectRegistryEff

data EffectRegistryEff id cb comp
  = Register id cb (() -> comp)
  | Obtain id (cb -> comp)
  deriving (Functor)

register :: id -> cb -> Eff (EffectRegistryEff id cb :+: effs) ()
register id callback = Impure $ L $ Register id callback Pure

obtain :: id -> Eff (EffectRegistryEff id cb :+: effs) cb
obtain id = Impure $ L $ Obtain id Pure

type Registry id cb = Map id cb

runEffectRegistryEff :: 
    (Functor effs, Ord id) =>
    Eff (
        EffectRegistryEff id cb 
        :+: effs) a ->
    Eff effs (a, Registry id cb)
runEffectRegistryEff = flip runEffectRegistryEff' empty

runEffectRegistryEff' :: 
    (Functor effs, Ord id) => 
    Eff (
        EffectRegistryEff id cb 
        :+: effs) a -> 
    Registry id cb ->
    Eff effs (a, Registry id cb)
runEffectRegistryEff' comp registry = case comp of
  Pure x -> Pure (x, registry)
  Impure (L op) -> case op of
    Register id cb cont -> runEffectRegistryEff' (cont ()) (Data.Map.insert id cb registry)
    Obtain id cont -> case Data.Map.lookup id registry of
      Just cb -> runEffectRegistryEff' (cont cb) registry
      Nothing -> error "Callback not found" -- I definitely need to handle it more gracefully, but not now
  Impure (R effs) -> Impure $ (`runEffectRegistryEff'` registry) <$> effs


-- I'll add separate state as it supposed to handle Dynamic implicitly for user
data VarsEff comp where
  Write :: String -> Dynamic -> (() -> comp) -> VarsEff comp
  Read :: Typeable a => String -> (a -> comp) -> VarsEff comp
  Mutate :: Typeable a => String -> (a -> a) -> (a -> comp) -> VarsEff comp

instance Functor VarsEff where -- Existantial vars is a mess
  fmap f (Write s d k) = Write s d (f . k)
  fmap f (Read s k) = Read s (f . k)
  fmap f (Mutate s g k) = Mutate s g (f . k)

type GlobalState = Map String Dynamic

readVar :: Typeable a => 
    String -> Eff (VarsEff :+: effs) a
readVar name = Impure $ L $ Read name Pure

writeVar :: Typeable a => 
    String -> a -> Eff (VarsEff :+: effs) ()
writeVar name val = Impure $ L $ Write name (toDyn val) Pure

mutateVar :: Typeable a => 
    String -> (a -> a) -> Eff (VarsEff :+: effs) a
mutateVar name fun = Impure $ L $ Mutate name fun Pure

runVarsEff :: (Functor effs, Typeable a) => 
    Eff (VarsEff :+: effs) a -> 
    Eff effs (a, GlobalState)
runVarsEff = flip runVarsEff' empty

runVarsEff' :: (Functor effs, Typeable a) => 
    Eff (
        VarsEff 
        :+: effs) a ->
    GlobalState ->
    Eff effs (a, GlobalState)
runVarsEff' comp state = case comp of
  Pure x -> Pure (x, state)
  Impure (L op) -> case op of
    Write name val cont -> runVarsEff' (cont ()) (Data.Map.insert name val state)
    Read name cont -> case Data.Map.lookup name state >>= fromDynamic of
      Just cb -> runVarsEff' (cont cb) state
      Nothing -> error "Cannot find variable with this name or cannot deduce type" -- I definitely need to handle it more gracefully, but not now
    Mutate name f cont -> case Data.Map.lookup name state >>= fromDynamic of
      Just cb ->
        let 
            res = f cb
            state' = Data.Map.insert name (toDyn res) state
        in runVarsEff' (cont res) state'
      Nothing -> error "Cannot find variable with this name or cannot deduce type" -- I definitely need to handle it more gracefully, but not now
  Impure (R effs) -> Impure $ (`runVarsEff'` state) <$> effs

-- mutateVar :: Typeable a => String -> (a -> a) -> Eff (VarsEff :+: effs) a
-- mutateVar name func = Impure $ L $ Mutate name func Pure
-- type Global = Map String Dynamic

-- Type sum alias of effects available in all button callbacks
type CallbackEffs = VarsEff :+: EmptyEff
-- Type alias just to make sure callback do not have return values
type Callback = Eff CallbackEffs ()

runCallbackEffs :: GlobalState -> Callback -> GlobalState
runCallbackEffs st callback = snd $ runEmptyEff $ runVarsEff' callback st

button :: (Functor effs) => Callback ->
    Eff (
        IncrementEff
        :+: GeneratorEff (UITree String)
        :+: EffectRegistryEff Int Callback
        :+: effs) ()
button callback = do
    id <- increment'
    -- traceShowM id
    register' id callback
    yieldEff' $ UILeaf ("Button " ++ show id)
    where yieldEff' = liftF . yieldEff
          increment' = increment
          register' a b = liftF . liftF $ register a b

