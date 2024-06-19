module Lib.Events where

import Control.Concurrent.STM
import Control.Monad (forM_)

data Event = Click Int
  deriving (Show, Eq)

type EventHandler = Event -> IO ()

newtype EventManager = EventManager { handlers :: TVar [EventHandler] }

initEventManager :: IO EventManager
initEventManager = EventManager <$> newTVarIO []

registerHandler :: EventManager -> EventHandler -> IO ()
registerHandler manager handler = atomically $ do
  hs <- readTVar (handlers manager)
  writeTVar (handlers manager) (handler : hs)

triggerEvent :: EventManager -> Event -> IO ()
triggerEvent manager event = do
  hs <- readTVarIO (handlers manager)
  forM_ hs ($ event)

simulateClick :: EventManager -> Int -> IO ()
simulateClick eventManager i = triggerEvent eventManager (Click i)
