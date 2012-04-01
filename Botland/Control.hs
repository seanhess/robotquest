{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Botland.Control where

import Botland.Types

import Control.Monad.IO.Class (liftIO)

import Data.Maybe (isNothing, fromJust)
import Data.DateTime (DateTime, addSeconds, getCurrentTime)

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


-- DETAILS --------------------------------------------------------

botDetails :: String -> Action IO (Either Fault Bot)
botDetails id = do
    d <- findOne (select ["_id" =: id] "bots")

    if (isNothing d) then
        return $ Left NotFound
    else do

    return $ Right $ fromDoc (fromJust d)

topKillers :: Action IO [Bot]
topKillers = do
    c <- find (select ["kills" =: ["$gt" =: 0]] "bots") {sort = ["kills" =: -1], limit = 20}
    ds <- rest c
    return $ map fromDoc ds

topSurvivors :: Action IO [Bot]
topSurvivors = do
    c <- find (select [] "bots") {sort = ["created" =: 1], limit = 20}
    ds <- rest c
    return $ map fromDoc ds


-- CREATION -------------------------------------------------------

-- we don't actually care what id you use as an MCP, but we provide a way to generate one here, so you don't have to hard-code it in your source and expose yourself to other people controlling your bots. Later we will store details about your mcp
createMcp :: Player -> Action IO Id
createMcp p = do
    id <- liftIO $ randomId
    let p' = p { mcpId = id }
    save "mcps" (toDoc p')
    updateHeartbeat id
    return $ Id id

createBot :: Game -> String -> Bot -> Action IO (Either Fault Id)
createBot g mcpId b = do
    id <- liftIO $ randomId
    time <- now
    let ub = b { botId = Just id, botMcpId = Just mcpId, created = time }

    let v = validPosition g (x b) (y b)

    if (not v) then 
        return $ Left InvalidPosition 
    else do

    -- this insert will fail if the location is occupied
    insert_ "bots" (toDoc ub)

    return $ Right $ Id id 



-- ACTIONS --------------------------------------------------------

--setAction :: String -> BotAction -> Action IO Ok
--setAction id a = do
--    modify (select ["_id" =: id] "bots") ["$set" =: ["action" =: (show a)]]
--    return Ok 

performCommand :: BotCommand -> Game -> String -> String -> Action IO (Either Fault Ok)
performCommand c g mcpId id = do
    updateHeartbeat mcpId
    case c of 
        BotCommand Move d -> moveAction g id d
        BotCommand Attack d -> attackAction g id d

moveAction :: Game -> String -> Direction -> Action IO (Either Fault Ok)
moveAction g id d = do

    -- I need to GET their current position
    doc <- findOne (select ["_id" =: id] "bots") {project = ["x" =: 1, "y" =: 1, "_id" =: 0]}

    if (isNothing doc) then
        return $ Left NotFound
    else do

    let bp = fromDoc (fromJust doc)
    let (Point x y) = move d bp

    let v = validPosition g x y
    if (not v) then 
        return $ Left InvalidPosition 
    else do 

    -- throws an error if someone is there
    modify (select ["_id" =: id] "bots") ["$set" =: ["x" =: x, "y" =: y]]

    return $ Right Ok

attackAction :: Game -> String -> Direction -> Action IO (Either Fault Ok)
attackAction g id d = do

    -- I need to GET their current position
    doc <- findOne (select ["_id" =: id] "bots") {project = ["x" =: 1, "y" =: 1, "_id" =: 0]}

    if (isNothing doc) then
        return $ Left NotFound
    else do

    let bp = fromDoc (fromJust doc)
    let (Point x y) = move d bp

    -- remove anybody there. Die sucka die
    -- see if anyone is there 
    targetDoc <- findOne (select ["x" =: x, "y" =: y] "bots") { project = ["_id" =: 1] }

    if (isNothing doc) then
        return $ Right Ok
    else do

    let tb = fromDoc (fromJust targetDoc) :: Bot

    -- nuke them!
    delete (select ["_id" =: botId tb] "bots")

    -- give the bot a kill
    modify (select ["_id" =: id] "bots") ["$inc" =: ["kills" =: 1]]

    return $ Right Ok





-- movement helpers --
move :: Direction -> Point -> Point
move d (Point x y) = case d of
    DLeft -> Point (x-1) y
    DRight -> Point (x+1) y
    DUp -> Point x (y-1)
    DDown -> Point x (y+1)


-- CLEANUP ---------------------------------------------------------

-- save when the mcp last completed an action
updateHeartbeat :: String -> Action IO ()
updateHeartbeat pid = do
    time <- now
    modify (select ["_id" =: pid] "mcps") ["$set" =: ["heartbeat" =: time]]

cleanupMcp :: String -> Action IO Ok
cleanupMcp mcpId = do
    liftIO $ putStrLn ("Cleaning Up " ++ mcpId)
    delete (select ["mcpId" =: mcpId] "bots")
    delete (select ["_id" =: mcpId] "mcps")
    return Ok

cleanupBot :: String -> String -> Action IO Ok
cleanupBot mcpId botId = do
    updateHeartbeat mcpId
    delete (select ["_id" =: botId] "bots")
    return Ok

cleanupInactives :: Integer -> Action IO Ok
cleanupInactives delay = do

    time <- now
    let tenSecondsAgo = addSeconds (-delay) time

    c <- find (select ["heartbeat" =: ["$lt" =: tenSecondsAgo]] "mcps")
    docs <- rest c

    let mcpIds = map mcpId $ map fromDoc docs

    mapM_ cleanupMcp mcpIds 
    return Ok



-- THE WORLD -------------------------------------------------------

locations :: Action IO [Bot]
locations = do
    c <- find (select [] "bots") {project = ["mcpId" =: 0]}
    bs <- rest c
    --return bs
    return $ map fromDoc bs 




-- HELPERS ----------------------------------------------------------

now :: Action IO DateTime
now = liftIO $ getCurrentTime

randomId :: IO String
randomId = do
    i <- randomIO
    return $ intToHex i

intToHex :: Int -> String
intToHex i = showIntAtBase 16 intToDigit (abs i) "" 

validPosition :: Game -> Int -> Int -> Bool
validPosition g x y = 0 <= x && x < (width g) && 0 <= y && y < (height g)
