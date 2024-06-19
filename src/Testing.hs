module Testing where

import Control.Monad (replicateM_)
import Data.Bool (bool)
import Debug.Trace (traceIO, traceShow, traceShowM, traceM)
import Lib.Effects
import Lib.Events
import Data.Map
import Data.IORef
import Data.Dynamic

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM cond body = cond >>= bool (pure ()) (body >> whileM cond body)

facEff :: Int -> ([String], Either String Int)
facEff n =
    applyEffects $
        fst <$> flip runStateEff (1 :: Int, n) do
          input <- snd <$> getEff
          if input < 0
            then throwEff' "Invalid Input"
            else do
              whileM ((> 1) . snd <$> getEff) do
                (res, n') <- getEff
                logEff' $ show res
                putEff (res * n', n' - 1)
              fst <$> getEff
  where
    throwEff' = liftF . throwEff
    logEff' = liftF . liftF . logEff
    applyEffects =
        runEmptyEff . runLoggerEff . runExceptionEff

uiTreeEff' :: UITree String
uiTreeEff' = do
  applyEffects do
    nestedEff "Root" do
      leafEff "Apple"
      leafEff "Maple"
    leafEff "Grape"
  where
    applyEffects = outer . runEmptyEff . runGeneratorEff
    outer x = if length x == 1 then head x else UINode mempty x


uiTreeButtons :: ([UITree String], Registry Int Callback)
uiTreeButtons = do
    applyEffects $ do
        leafEff' "Apple"
        button do 
            writeVar "a" (0 :: Int)
            return ()
        button do 
            _ <- mutateVar "a" ((+ 1) :: (Int -> Int))
            return ()
        button do 
            _ <- mutateVar "a" ((+ 3) :: (Int -> Int))
            return ()
        leafEff' "Apple2"
    where
        leafEff' = liftF . leafEff
        applyEffects = runEmptyEff . runEffectRegistryEff . runGeneratorEff . runIncrementEff


clickAt :: IORef GlobalState -> IORef (Registry Int Callback) -> Int -> IO ()
clickAt stateRef regRef i = do
    state <- readIORef stateRef
    reg <- readIORef regRef
    let callback = reg ! i
        state' = runCallbackEffs state callback
    writeIORef stateRef state'

uiLoop :: EventManager -> IORef GlobalState -> IORef (Registry Int Callback) -> IO ()
uiLoop eventManager stateRef registryRef = do
    let (tree, registry) = uiTreeButtons
    writeIORef registryRef registry
    simulateClick eventManager 1 -- TODO remove later
    uiLoop eventManager stateRef registryRef

uiEntryPoint :: IO ()
uiEntryPoint = do
  let (tree, registry) = uiTreeButtons
  stateRef <- newIORef empty
  registryRef <- newIORef registry
  eventManager <- initEventManager

  let clickHandler (Click i) = do
        putStrLn $ "Click event triggered for button: " ++ show i
        clickAt stateRef registryRef i
        state <- readIORef stateRef
        let looked = (Data.Map.lookup "a" state >>= fromDynamic :: Maybe Int)
        putStrLn $ show looked

  registerHandler eventManager clickHandler

  simulateClick eventManager 0

  uiLoop eventManager stateRef registryRef
