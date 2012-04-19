
module Botland.Game where

import Botland.Types

import Control.Concurrent (forkIO, threadDelay)

import qualified Data.Map as M
import Data.Map (Map, insert, delete, lookup, empty)
import Data.Maybe (fromMaybe, fromJust, isJust)

import Prelude hiding (lookup)

-- STORAGE: units (bots and blocks), points (x, y, unitId), commands (action, direction, botId),
-- PULL THEM OUT: get all points, get all units, create Map Location Unit, create Map id Unit, [(id, Commands)], for each one, send it along to game. 
-- STORE THEM AGAIN: save all bots (?), save the world locations

-- If I'm serializing them like this, I might as well have a single thing, no?

-- fields: "world"
-- wait, I don't need to serialize it at all. I can create it, and pass it around in my loop!
-- then, I only need to save it so someone can request it. 

-- they have mvars ... ooh, fancy
-- why not eh?

-- naw, too complicated. It's easy to serialize it anyway
-- the easiest way to DUMP the data is to have an x, y on the bots themselves. I have to save the bots anyway.
-- so, I'll store the data on the bots, and have a parsing method that can return both

-- easy step one 

-- CREATION
-- 1. the client could just insert the thang
-- 2. or, have it here. store the command to create somewhere? (in a capped collection)
-- 3. Easiest: assume they have it in there already. THis is just handling movement. 

-- EASY
-- 1. allow them to create normally
-- 2. move a guy 
-- 3. make sure he's moved



-- [x] Save Commands
-- [x] Save bots
-- [x] Save positions
-- [ ] Load Commands (+Bot)
-- [x] Load World (+Bot)


type IdMap = Map String Bot


runTick :: Integer -> IO ()
runTick delay = do
    putStrLn "WOOT"
    threadDelay ((fromIntegral delay)*1000000)
    runTick delay



-- CONVERT IT ALL
doStuff :: IO ()
doStuff = do
  putStrLn "HI"
    -- you have the commands (Bot, BotCommand)
    -- you have the bots [Bot]
    -- you have the field Field


-- 1. collect all the info (Bot, BotCommand, Point)
-- 2. then it's easy to fold it over

{-botPoint :: Bot -> Point-}
{-botPoint f (b, c) = (b, c, p)-}
    {-where p = fromMaybe InvalidPoint (lookup b f)-}

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

