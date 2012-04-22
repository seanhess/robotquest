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
gameTick game = do

    removeDeadBots
    state <- loadState
    let newState = processActions game state
    liftIO $ print newState
    saveState newState
    clearCommands


-- these are the things we get from the database, convert and prepare them!
processActions :: GameInfo -> Game -> Game
processActions g gs = 
  let bots = toBots gs
  in foldr (foldField g) gs bots

foldField :: GameInfo -> Bot -> Game -> Game
foldField g b gs = 
    let p = point b in
    case command b of 
        Nothing -> gs
        Just c -> runAction g b p c gs

-- route the different action functions
runAction :: GameInfo -> Bot -> Point -> BotCommand -> Game -> Game
runAction g b p (BotCommand Move d) gs = moveAction g b p d gs
runAction g b p (BotCommand Attack d) gs = attackAction g b p d gs
runAction _ _ _ _ gs = gs

-- get moving working without a monad, then switch
moveAction :: GameInfo -> Bot -> Point -> Direction -> Game -> Game 
moveAction g b start d state = 

    let dest = destination d start in

    if not (validPosition g dest) then state else
    if isOccupied dest state then state else

    let b' = b { point = dest }
        s' = clearPoint start state
    in setBot b' s'

attackAction :: GameInfo -> Bot -> Point -> Direction -> Game -> Game
attackAction g b p d s = 
    
    let dest = destination d p in

    case atPoint dest s of
      Nothing -> s
      Just victim -> 
          let s' = setBot (victim { botState = Dead }) s
              k = (kills b) + 1
          in setBot (b { kills = k}) s'
          
    -- TODO give kills

{-g b start d f-}
    {-let dest = move d start in-}
    {-case lookup dest f of-}
        {-Nothing -> w-}
        {-Just target -> delete dest w-}



removeMisses :: [(Maybe a)] -> [a]
removeMisses ms = map fromJust $ filter isJust $ ms

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

