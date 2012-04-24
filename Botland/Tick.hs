{-

Botland Game Timer: gameTick runs once per game tick, updating the world and saving it out

TODO 

-}


module Botland.Tick where

import Botland.Control
import Botland.GameState
import Botland.Types

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execState)
import Control.Monad (when, unless)

import qualified Data.Map as M
import Data.Map (Map, insert, delete, lookup, empty)
import Data.Maybe (fromMaybe, fromJust, isJust)

import Database.MongoDB (Action, Failure)

import Prelude hiding (lookup)

import System.CPUTime (getCPUTime)

type IdMap = Map String Bot

gameInfo :: GameInfo
gameInfo = GameInfo 25 20 1000 

runTick :: GameInfo -> (Action IO () -> IO (Either Failure ())) -> IO ()
runTick g db = do

    let delayms = tick g

    startTime <- getCPUTime -- picoseconds

    db $ gameTick g

    -- we need to wait 1s minus the time taken (setInterval, where are you!)
    endTime <- getCPUTime
    let elapsedps = endTime - startTime :: Integer
        durationµs = round $ ((fromInteger elapsedps) / 1000000) :: Integer
        waitµs = (delayms * 1000) - durationµs

    threadDelay $ fromIntegral waitµs

    runTick g db


gameTick :: GameInfo -> Action IO ()
gameTick info = do
    removeDeadBots
    bots <- allBots
    let state = emptyGame info
    let newState = execState (processActions bots) state 
    liftIO $ print newState
    mapM_ updateBot $ toBots newState
    clearCommands

processActions :: [Bot] -> GameState ()
processActions bots = do
    addBots bots
    mapM_ runBotCommand bots 

runBotCommand :: Bot -> GameState ()
runBotCommand b = case (command b) of
    Nothing -> return ()
    Just c -> runAction b (point b) c

-- route the different action functions
runAction :: Bot -> Point -> BotCommand -> GameState ()
runAction b p (BotCommand Move d) = moveAction b p d
runAction b p (BotCommand Attack d) = attackAction b p d
runAction _ _ _ = return ()

-- get moving working without a monad, then switch
moveAction :: Bot -> Point -> Direction -> GameState ()
moveAction b start d = do

    let dest = destination d start

    skipIf (not.(onMap dest)) $ do
    skipIf (isOccupied dest) $ do

    clear start
    update $ b { point = dest }

attackAction :: Bot -> Point -> Direction -> GameState () 
attackAction b p d = do
    
    let dest = destination d p

    mv <- atPoint dest
    case mv of
      Nothing -> return ()
      Just victim -> do
          let k = (kills b) + 1
          update $ victim { botState = Dead }
          update $ b { kills = k }
          
-- converts a command document into a (Bot, BotCommand)
botById :: IdMap -> String -> Maybe Bot
botById m id = lookup id m 

-- creates a map of bot id to bot, for use in the other functions
idMap :: [Bot] -> IdMap
idMap bs = foldr a empty bs
    where a b m = insert (botId b) b m

{-toField :: [Bot] -> Field-}
{-toField bs = foldr a empty bs -}
    {-where a b f = insert (Point (x b) (y b)) b f-}

-- Gives you the point in a given direction
destination :: Direction -> Point -> Point
destination d (Point x y) = case d of
    DLeft -> Point (x-1) y
    DRight -> Point (x+1) y
    DUp -> Point x (y-1)
    DDown -> Point x (y+1)

