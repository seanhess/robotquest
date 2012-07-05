module Botland.GameState where

import Botland.Types

import Control.Monad.State
import Control.Monad (when, unless)

import Data.Map (Map, insert, delete, lookup, elems, empty)
import Data.Maybe (isJust)

import Prelude hiding (lookup)


-- GAME STATE ------------------------------------------------

type GameState a = State Game a

data Game = Game { info :: GameInfo, bots :: Map String Bot, points :: Map Point GameObject, changes :: [StateChange] } deriving (Show)

data StateChange = AddBlock Block     -- add a block at point x
                 | DeleteBlock Block  --

-- what if someone moves onto point x, then someone attacks that point. 
    -- final state: person x dead at that point
    -- but we don't want to SAVE both of those
-- even if we do updates we probably don't want to do updates
-- oh, just flag things as modified!
-- right now we save EVERYTHING I think

-- but I only NEED to save:
  -- bots who move (mark them as changed)
  -- bots who die (mark them as changed)
  -- new blocks
  -- deleted blocks
  -- kills

  -- what if blocks had ids?
    -- then you could keep points :: Map Point String
    -- updated could be a set of updated ids
    -- then you could pull them out of objects :: Map String GameObject
    -- and see their final state. 
        -- block REMOVED

-- what if there were a MUCH simpler way to do this?

-- FIELD: Map Point String <-- just save it straight up
-- OBJECTS: <-- a thang full of objects by id

-- getObjects: 1. get the field, 2. get each id, 3. associate and send back

-- UPDATES: could be by POSITION.

-- getObjects: could just return the ids of what is where
-- object/info: could return the info
-- or you could join them like a friendly person

{-

1. store the field as its own object. save the whole thing. blocks will just plain disappear
    -- don't give IDS in the db

2. 


-}



emptyGame :: GameInfo -> Game
emptyGame i = Game i empty empty

addObjects :: [GameObject] -> GameState ()
addObjects os = mapM_ update os

addBots :: [Bot] -> GameState ()
addBots bs = mapM_ update bs

toBots :: Game -> [Bot]
toBots gs = elems (bots gs)

insertObject :: GameObject -> Game -> Game
insertObject (BotObject b) (Game i bs ps) =
    let id = botId b
        bs' = insert id b bs
        ps' = insert (point b) id ps
    in Game i bs' ps'
insertObject (BlockObject b) (Game i bs ps) =

update :: GameObject -> GameState ()
update b = do
    g <- get
    put $ insertBot b g

isOccupied :: Point -> Game -> Bool
isOccupied p g = isJust $ lookup p (points g) 

atPoint :: Point -> GameState (Maybe Bot)
atPoint p = do
    s <- get
    return $ atPoint' p s

atPoint' :: Point -> Game -> Maybe Bot
atPoint' p s = do
    let bs = bots s
        ps = points s

    id <- lookup p ps
    b <- lookup id bs
    return b

onMap :: Point -> Game -> Bool
onMap p (Game info _ _) = validPosition info p

clear :: Point -> GameState ()
clear p = do
    s <- get
    let ps = delete p (points s)
    put $ s { points = ps }

validPosition :: GameInfo -> Point -> Bool
validPosition g (Point x y) = 0 <= x && x < (width g) && 0 <= y && y < (height g)

skipIf :: (Game -> Bool) -> GameState () -> GameState ()
skipIf p k = do
  g <- get
  unless (p g) k


