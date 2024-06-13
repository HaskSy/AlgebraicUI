module Testing where

import Control.Monad (replicateM_)
import Data.Bool (bool)
import Debug.Trace (traceIO, traceShow, traceShowM, traceM)
import Lib.Effects

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


uiTreeButtons :: [UITree String]
uiTreeButtons = do
    applyEffects $ do
        leafEff' "Apple"
        -- button
        -- button
        -- button 
        leafEff' "Apple2"
    where
        leafEff' = liftF . leafEff
        applyEffects = runEmptyEff . runGeneratorEff . runIncrementEff


uiLoop :: IO ()
uiLoop = do
    print uiTreeButtons
    uiLoop

