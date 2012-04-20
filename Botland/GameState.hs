module Botland.GameState where

import Botland.Types

import Data.Map (Map, insert, delete, lookup, elems, empty)
import Data.Maybe (isJust)

import Prelude hiding (lookup)

-- GAME STATE ------------------------------------------------

data GameState = GameState { bots :: Map String Bot, points :: Map Point String } deriving (Show)

emptyState :: GameState
emptyState = GameState empty empty

fromBots :: [Bot] -> GameState
fromBots bs = foldr setBot emptyState bs

toBots :: GameState -> [Bot]
toBots gs = elems (bots gs)

setBot :: Bot -> GameState -> GameState
setBot b (GameState bs ps) = 
    let id = botId b
        bs' = insert id b bs
        ps' = insert (point b) id ps
    in GameState bs' ps'

isOccupied :: Point -> GameState -> Bool
isOccupied p s = isJust $ lookup p (points s)

atPoint :: Point -> GameState -> Maybe Bot
atPoint p s = do
    let bs = bots s
        ps = points s

    id <- lookup p ps
    b <- lookup id bs
    return b

clearPoint :: Point -> GameState -> GameState
clearPoint p s = 
    let ps = delete p (points s)
    in s { points = ps }


-- TODO Make a freaking monad! It'll be EPIC


