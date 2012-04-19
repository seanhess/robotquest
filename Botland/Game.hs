{-

Botland Game Timer: gameTick runs once per game tick, updating the world and saving it out


-}


module Botland.Game where

import Botland.Types
import Botland.Control

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map as M
import Data.Map (Map, insert, delete, lookup, empty)
import Data.Maybe (fromMaybe, fromJust, isJust)

import Database.MongoDB (Action, Failure)

import Prelude hiding (lookup)

import System.CPUTime (getCPUTime)

type IdMap = Map String Bot

runTick :: Integer -> (Action IO () -> IO (Either Failure ())) -> IO ()
runTick delay db = do

    startTime <- getCPUTime -- picoseconds

    db $ gameTick

    -- we need to wait 1s minus the time taken (setInterval, where are you!)
    endTime <- getCPUTime
    let elapsedps = endTime - startTime :: Integer
        durationµs = round $ ((fromInteger elapsedps) / 1000000) :: Integer
        waitµs = (delay * 1000000) - durationµs

    {-putStrLn "WAITING FOR"-}
    {-print waitµs-}
    threadDelay $ fromIntegral waitµs

    runTick delay db


gameTick :: Action IO ()
gameTick = do
    bots <- allBots
    commands <- allCommands

    let newField = processActions bots commands
    liftIO $ print newField
    saveField newField
    clearCommands


-- 1. collect all the info (Bot, BotCommand, Point)
-- 2. then it's easy to fold it over

-- these are the things we get from the database, convert and prepare them!
processActions :: [Bot] -> [(String, BotCommand)] -> Field
processActions bs ics = foldl foldField field bcps
  where field = toField bs
        idmap = idMap bs
        cs = map snd ics :: [BotCommand]
        ids = map fst ics :: [String]
        mbs = map (botById idmap) ids :: [Maybe Bot]
        mps = map botPoint mbs :: [Maybe Point]
        bcps = zip3 mbs cs mps :: [(Maybe Bot, BotCommand, Maybe Point)]

foldField :: Field -> (Maybe Bot, BotCommand, Maybe Point) -> Field
foldField f (Nothing, _, _) = f
foldField f (_, _, Nothing) = f
foldField f (mb, c, mp) = runAction b p c f
    where p = fromJust mp
          b = fromJust mb

  -- HAVE: [Maybe Bot], [BotCommand]
  -- + [Maybe Point]

  -- NEED: [(Bot, Maybe BotCommand, Maybe Point)]
  -- I can use zip for that, quite easily.
  -- just need functions that return the arrays in the same order?
  -- which one do we use the commands for?

-- route the different action functions
runAction :: Bot -> Point -> BotCommand -> Field -> Field
runAction b p (BotCommand Move d) w = moveAction b p d w
runAction b p (BotCommand Attack d) w = attackAction b p d w
runAction _ _ _ w = w

-- get moving working without a monad, then switch
moveAction :: Bot -> Point -> Direction -> Field -> Field 
moveAction b start d w = case lookup dest w of 
      Just u -> w
      Nothing -> delete start $ insert dest b w
    where dest = move d start

attackAction :: Bot -> Point -> Direction -> Field -> Field
attackAction = undefined







removeMisses :: [(Maybe a)] -> [a]
removeMisses ms = map fromJust $ filter isJust $ ms

-- converts a command document into a (Bot, BotCommand)
botById :: IdMap -> String -> Maybe Bot
botById m id = lookup id m 

botPoint :: Maybe Bot -> Maybe Point
botPoint Nothing = Nothing

botPoint mb = Just $ Point (x b) (y b) 
    where b = fromJust mb

-- creates a map of bot id to bot, for use in the other functions
idMap :: [Bot] -> IdMap
idMap bs = foldr a empty bs
    where a b m = insert (botId b) b m

toField :: [Bot] -> Field
toField bs = foldr a empty bs 
    where a b f = insert (Point (x b) (y b)) b f






move :: Direction -> Point -> Point
move d (Point x y) = case d of
    DLeft -> Point (x-1) y
    DRight -> Point (x+1) y
    DUp -> Point x (y-1)
    DDown -> Point x (y+1)

