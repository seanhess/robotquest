module Botland.GameState where

import Botland.Types

import Control.Monad.State

import Data.Map (Map, insert, delete, lookup, elems, empty)
import Data.Maybe (isJust)

import Prelude hiding (lookup)


-- GAME STATE ------------------------------------------------

type GameState a = State Game a

data Game = Game { info :: GameInfo, bots :: Map String Bot, points :: Map Point String } deriving (Show)

emptyGame :: GameInfo -> Game
emptyGame i = Game i empty empty

addBots :: [Bot] -> GameState ()
addBots bs = mapM_ update bs

toBots :: Game -> [Bot]
toBots gs = elems (bots gs)

insertBot :: Bot -> Game -> Game
insertBot b (Game i bs ps) =
    let id = botId b
        bs' = insert id b bs
        ps' = insert (point b) id ps
    in Game i bs' ps'

update :: Bot -> GameState ()
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
  when (p g) k

  




