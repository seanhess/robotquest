{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DoAndIfThenElse #-}

module Botland.Control where

import Botland.Types
import Botland.GameState

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)

import Data.Map (assocs)
import Data.Maybe (isNothing, fromJust, isJust)
import Data.DateTime (DateTime, addSeconds, getCurrentTime)

import Database.MongoDB hiding (Field)

import Web.Scotty (ActionM(..))

-- randomId stuff 
import System.Random (randomIO)
import Numeric (showIntAtBase) 
import Data.Char (intToDigit)


-- SETUP ----------------------------------------------------------
connectMongo :: IO (Pipe) 
connectMongo = runIOE $ connect (host "127.0.0.1")

ensureIndexes :: Action IO ()
ensureIndexes = do

    -- I query by location when someone spawns
    ensureIndex (Index "bots" ["x" =: 1, "y" =: 1] "xy" False False)

    -- for leaderboards
    ensureIndex (Index "bots" ["kills" =: -1] "kills" False False)
    ensureIndex (Index "bots" ["created" =: 1] "created" False False)

    -- for ownership
    ensureIndex (Index "bots" ["playerId" =: 1] "bot_player_id" False False)

    -- for commands
    ensureIndex (Index "bots" ["speed" =: -1] "bot_speed" False False)

    -- to look up a player. Unique index. Can't have two players with the same name
    ensureIndex (Index "players" ["name" =: 1] "player_name" True True)

    -- for cleanup
    ensureIndex (Index "players" ["heartbeat" =: 1] "player_heartbeat" False False)

botOwner :: String -> String -> Action IO Bool
botOwner pid botId = do
    m <- findOne (select ["_id" =: botId, "playerId" =: pid] "bots") {project = ["_id" =: 1]}
    return $ isJust m


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
    c <- find (select ["kills" =: ["$gt" =: 0]] "bots") {sort = ["kills" =: -1], limit = 10}
    ds <- rest c
    return $ map fromDoc ds

topSurvivors :: Action IO [Bot]
topSurvivors = do
    c <- find (select [] "bots") {sort = ["created" =: 1], limit = 10}
    ds <- rest c
    return $ map fromDoc ds



-- PLAYER ---------------------------------------------------------

getPlayerByName :: String -> Action IO (Maybe Player)
getPlayerByName n = getPlayer ["name" =: n]

getPlayerById :: String -> Action IO (Maybe Player)
getPlayerById id = getPlayer ["_id" =: id]

{-getPlayer :: Field -> Action IO (Maybe Player)-}
getPlayer s = do
    md <- findOne (select s "players") {project = ["_id" =: 0]}
    case md of
        Nothing -> return Nothing
        Just d -> return $ Just $ fromDoc d
    
-- we provide a random id. It is your secret id from now on, and you use it to control your bots
createPlayer :: Player -> Action IO (Either Fault Id)
createPlayer p = do

    let name = (playerName p)

    active <- isPlayerNameActive name

    if (active) then
      return $ Left $ Fault "Player name already exists and is still active"
    else do

    -- remove any player of that name (if they're still there, they are inactive)
    deletePlayerByName name 

    -- now, register the new player with a brand new id
    id <- randomId
    let p' = p { playerId = id }
    save "players" (toDoc p')
    updateHeartbeat id
    return $ Right $ Id id

isPlayerNameActive :: String -> Action IO Bool
isPlayerNameActive name = do
    c <- count (select ["name" =: name, "active" =: True] "players")
    return (c > 0)

-- CREATION -------------------------------------------------------

createBot :: GameInfo -> String -> Bot -> Action IO (Either Fault Id)
createBot g pid b = do
    id <- randomId
    time <- now

    -- get player information
    mp <- getPlayerById pid
    if (isNothing mp) then
        return $ Left $ Fault "Player Not Found"
    else do

    let p = fromJust mp
        pn = playerName p

    let ub = b { botId = id, botPlayerId = pid, player = pn, created = time }

    -- check is valid position
    let v = validPosition g (point b)
    if (not v) then 
        return $ Left InvalidPosition 
    else do

    -- check is occupied
    occupied <- locationOccupied (point ub)
    if (occupied) then
        return $ Left InvalidPosition
    else do

    insert_ "bots" (toDoc ub)

    return $ Right $ Id id 

locationOccupied :: Point -> Action IO Bool
locationOccupied p = do
    c <- count (select ["x" =: (x p), "y" =: (y p), "state" =: Active] "bots")
    return (c > 0)


-- GAME STATE -----------------------------------------------------

botsBySpeed :: Action IO [Bot]
botsBySpeed = do
    c <- find (select [] "bots") {sort = ["speed" =: -1]}
    docs <- rest c
    return $ map fromDoc docs

updateBot :: Bot -> Action IO ()
updateBot b = do
    let p = point b
    modify (select ["_id" =: botId b] "bots") ["$set" =: ["x" =: x p, "y" =: y p, "state" =: botState b, "kills" =: kills b]]

clearCommands :: Action IO ()
clearCommands = do
    modify (select [] "bots") ["$unset" =: ["command" =: 1]]

removeDeadBots :: Action IO ()
removeDeadBots = do
    delete (select ["state" =: Dead] "bots")

-- ACTIONS --------------------------------------------------------

setCommand :: BotCommand -> GameInfo -> String -> String -> Action IO ()
setCommand c g pid id = do
    updateHeartbeat pid

    -- later, speed will be set via items. For now, let's set it randomly so bots
    -- at the top don't have an advantage
    s <- randomSpeed

    modify (select ["_id" =: id] "bots") ["$set" =: ["command" =: c, "speed" =: s]]


-- CLEANUP ---------------------------------------------------------

-- save when the player last completed an action
-- mark them as active
updateHeartbeat :: String -> Action IO ()
updateHeartbeat pid = do
    time <- now
    modify (select ["_id" =: pid] "players") ["$set" =: ["heartbeat" =: time, "active" =: True]]

cleanupPlayer :: String -> Action IO Ok
cleanupPlayer id = do
    liftIO $ putStrLn ("Cleaning Up " ++ id)
    delete (select ["playerId" =: id] "bots")
    
    -- Marks the player as inactive, so someone can create over it if they want
    modify (select ["_id" =: id] "players") ["$set" =: ["active" =: False]]

    return Ok

-- deletes a player by name so you can re-register the name
deletePlayerByName :: String -> Action IO ()
deletePlayerByName name = do
    delete (select ["name" =: name] "players")

cleanupBot :: String -> Action IO Ok
cleanupBot botId = do
    delete (select ["_id" =: botId] "bots")
    return Ok

cleanupInactives :: Integer -> Action IO Ok
cleanupInactives delay = do

    time <- now
    let cutoffTime = addSeconds (-delay) time

    -- query for inactive players
    c <- find (select ["heartbeat" =: ["$lt" =: cutoffTime], "active" =: True] "players")
    docs <- rest c

    let ids = map playerId $ map fromDoc docs

    mapM_ cleanupPlayer ids 
    return Ok

    



-- THE WORLD -------------------------------------------------------

objects :: Action IO [Bot]
objects = do
    c <- find (select [] "bots") {project = ["id" =: 1, "x" =: 1, "y" =: 1, "name" =: 1, "sprite" =: 1, "player" =: 1, "state" =: 1, "kills" =: 1, "created" =: 1]}
    bs <- rest c
    return $ map fromDoc bs




-- HELPERS ----------------------------------------------------------

now :: Action IO DateTime
now = liftIO $ getCurrentTime

randomId :: Action IO String
randomId = do
    i <- liftIO $ randomIO
    return $ intToHex i

randomSpeed :: Action IO Int
randomSpeed = do
    i <- liftIO $ randomIO
    return i

intToHex :: Int -> String
intToHex i = showIntAtBase 16 intToDigit (abs i) "" 
