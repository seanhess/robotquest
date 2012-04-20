module Botland.GameState where

import Botland.Types

import Data.Map (Map, insert, delete, lookup, elems, empty)
import Data.Maybe (isJust)

import Prelude hiding (lookup)

-- GAME STATE ------------------------------------------------

data GameState = GameState { bots :: Map String Bot, points :: Map Point Bot } deriving (Show)

emptyState :: GameState
emptyState = GameState empty empty

fromBots :: [Bot] -> GameState
fromBots bs = foldr fold emptyState bs
    where fold b s = 
            let bs = insert (botId b) b (bots s)
                ps = insert (point b) b (points s)
            in s { points = ps, bots = bs }

toBots :: GameState -> [Bot]
toBots gs = elems (bots gs)

isOccupied :: Point -> GameState -> Bool
isOccupied p gs = isJust $ lookup p (points gs)

clearPoint :: Point -> GameState -> GameState
clearPoint p s = 
    let ps = delete p (points s)
    in s { points = ps }

setPoint :: Bot -> Point -> GameState -> GameState
setPoint b p s = 
    let b' = b { point = p }
        bs = insert (botId b) b' (bots s)
        ps = insert p b' (points s)
    in s { points = ps, bots = bs }


