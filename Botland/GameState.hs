module Botland.GameState where

import Botland.Types

import Control.Monad.State

import Data.Map (Map, insert, delete, lookup, elems, empty)
import Data.Maybe (isJust)

import Prelude hiding (lookup)

-- GAME STATE ------------------------------------------------

type GameState a = State Game a

data Game = Game { bots :: Map String Bot, points :: Map Point String } deriving (Show)

emptyState :: Game
emptyState = Game empty empty

addBots :: [Bot] -> GameState
addBots bs = do
    foldr setBot emptyState bs

toBots :: Game -> [Bot]
toBots gs = elems (bots gs)

setBot :: Bot -> GameState ()
setBot b = do
    (Game bs ps) <- get
    let id = botId b
        bs' = insert id b bs
        ps' = insert (point b) id ps
    put $ Game bs' ps'

isOccupied :: Point -> Game -> Bool
isOccupied p s = isJust $ lookup p (points s)

atPoint :: Point -> Game -> Maybe Bot
atPoint p s = do
    let bs = bots s
        ps = points s

    id <- lookup p ps
    b <- lookup id bs
    return b

clearPoint :: Point -> Game -> Game
clearPoint p s = 
    let ps = delete p (points s)
    in s { points = ps }


-- TODO Make a freaking monad! It'll be EPIC


