{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Botland.Control where

import Botland.Types

import Control.Monad.IO.Class (liftIO)

import Database.MongoDB

import Web.Scotty (ActionM(..))

-- randomId stuff 
import System.Random (randomIO)
import Numeric (showIntAtBase) 
import Data.Char (intToDigit)



ensureIndexes :: Action IO ()
ensureIndexes = do
    ensureIndex (Index "bots" ["x" =: 1, "y" =: 1] "xy" True True)

botOwner :: String -> String -> Action IO Bool
botOwner mcpId botId = do
    n <- count $ select ["mcpId" =: mcpId, "_id" =: botId] "bots"
    return (n > 0)

createMcp :: ActionM Id
createMcp = do
    id <- liftIO $ randomId
    return $ Id id

-- use a mongo thang
-- if it fails, it exists EVERYTHING with either Failure a
createBot :: String -> Bot -> Action IO Id
createBot mcpId b = do
    id <- liftIO $ randomId
    let ub = b { botId = Just id, mcpId = Just mcpId }

    -- TODO: validate starting location for boundaries

    insert_ "bots" (toDoc ub)
    return $ Id id 

-- Just saves an action
setAction :: String -> BotAction -> Action IO Ok
setAction id a = do
    modify (select ["_id" =: id] "bots") ["$set" =: ["action" =: (show a)]]
    return Ok 


locations :: Action IO [Bot]
locations = do
    c <- find (select [] "bots") {project = ["mcpId" =: 0]}
    bs <- rest c
    --return bs
    return $ map fromDoc bs 



-- most likely you want this
randomId :: IO String
randomId = do
    i <- randomIO
    return $ intToHex i

intToHex :: Int -> String
intToHex i = showIntAtBase 16 intToDigit (abs i) "" 


--validPoint :: FieldInfo -> Point -> Bool
--validPoint f p = xmin <= px && px < xmax && ymin <= py && py < ymax
--    where px = x p
--          py = y p
--          xmin = x $ fieldStart f
--          xmax = xmin + (width $ fieldSize f)
--          ymin = y $ fieldStart f
--          ymax = ymin + (height $ fieldSize f)

