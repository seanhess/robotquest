{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Botland.Control where

import Botland.Types hiding (Direction(..))

import Control.Monad.IO.Class (liftIO)

import Database.MongoDB

import Web.Scotty (ActionM(..))

-- randomId stuff 
import System.Random (randomIO)
import Numeric (showIntAtBase) 
import Data.Char (intToDigit)


-- SETUP ----------------------------------------------------------
ensureIndexes :: Action IO ()
ensureIndexes = do
    ensureIndex (Index "bots" ["x" =: 1, "y" =: 1] "xy" True True)

botOwner :: String -> String -> Action IO Bool
botOwner mcpId botId = do
    n <- count $ select ["mcpId" =: mcpId, "_id" =: botId] "bots"
    return (n > 0)


-- CREATION -------------------------------------------------------

-- we don't actually care what id you use as an MCP, but we provide a way to generate one here, so you don't have to hard-code it in your source and expose yourself to other people controlling your bots. Later we will store details about your mcp
createMcp :: ActionM Id
createMcp = do
    id <- liftIO $ randomId
    return $ Id id

createBot :: Game -> String -> Bot -> Action IO (Either Fault Id)
createBot g mcpId b = do
    id <- liftIO $ randomId
    let ub = b { botId = Just id, mcpId = Just mcpId }

    let v = validPosition g (x b) (y b)

    if (not v) then 
        return $ Left InvalidPosition 
    else do

    -- this insert will fail if the location is occupied
    insert_ "bots" (toDoc ub)

    return $ Right $ Id id 



-- ACTIONS --------------------------------------------------------

setAction :: String -> BotAction -> Action IO Ok
setAction id a = do
    modify (select ["_id" =: id] "bots") ["$set" =: ["action" =: (show a)]]
    return Ok 





-- THE WORLD -------------------------------------------------------

locations :: Action IO [Bot]
locations = do
    c <- find (select [] "bots") {project = ["mcpId" =: 0]}
    bs <- rest c
    --return bs
    return $ map fromDoc bs 




-- HELPERS ----------------------------------------------------------

randomId :: IO String
randomId = do
    i <- randomIO
    return $ intToHex i

intToHex :: Int -> String
intToHex i = showIntAtBase 16 intToDigit (abs i) "" 

validPosition :: Game -> Int -> Int -> Bool
validPosition g x y = 0 <= x && x < (width g) && 0 <= y && y < (height g)
