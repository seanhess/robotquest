{-# LANGUAGE OverloadedStrings #-}

module Botland.Actions where

import Prelude hiding ((++))

import Botland.Helpers (uuid, l2b, b2l, (++))
import Botland.Types.Message (Fault(..))
import Botland.Types.Location (Field(..), Point(..), Size(..), Location(..), FieldInfo(..))
import Botland.Types.Unit (Spawn(..), UnitDescription(..), SpawnRequest(..))

import Control.Monad.IO.Class (liftIO)

import Database.Redis

import Data.Aeson (encode, decode)
import Data.ByteString.Char8 (pack, unpack, concat, ByteString(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.DateTime (getCurrentTime, formatDateTime, DateTime)
import qualified Data.Text.Lazy as T
import Data.Maybe (fromMaybe, fromJust)

import Debug.Trace (trace)


worldInfo :: FieldInfo
worldInfo = FieldInfo (Point 0 0) (Size 100 100)


-- the field for the whole world. Remove this eventually in favor of smaller fields
worldLocations :: Redis (Either Fault [Location])
worldLocations = do
    reply <- hgetall "world"  
    case reply of
        Left r -> return $ Left (Fault "Could not get world")
        Right m -> return $ Right (map toLocation m)

toLocation :: (ByteString, ByteString) -> Location
toLocation (ps, id) = Location p id
    where p = fromMaybe (Point 0 0) $ decode $ b2l ps

unitGetDescription :: ByteString -> Redis (Either Fault UnitDescription)
unitGetDescription uid = do
    reply <- get ("units:" ++ uid ++ ":description")
    case reply of
        Left _ -> return $ Left NotFound
        Right Nothing -> return $ Left NotFound
        Right (Just bs) -> do
            let ma = decode $ b2l bs :: Maybe UnitDescription
            case ma of
                Nothing -> return $ Left $ Fault "Could not parse description"
                Just a -> return $ Right a

-- spawning is relatively infrequent. 
-- it's ok if this is a little bit of an expensive operation?
-- or should I keep track of an index on every move?

-- when it starts: create the room?
-- set per room, the values are the locations?

unitSpawn :: SpawnRequest -> Redis (Either Fault Spawn)
unitSpawn sr = do
    id <- uuid
    token <- uuid

    -- everyone starts at the same place. You don't exist until you move off of it.
    -- create a spawnAt 
    let d = unitDescription sr 
    let p = requestedPoint sr

    liftIO $ print p
    liftIO $ print worldInfo


    -- LOCATION
    if (not $ validPoint worldInfo p) then 
        return $ Left $ Fault "Invalid spawn point"
    else do

    res <- claimLocation id p
    case res of 
        Left f -> return $ Left f
        Right False -> return $ Left $ Fault "could not claim space"
        Right True -> do
            -- save the actor information, its token, and its position
            set ("units:" ++ id ++ ":description") (l2b $ encode d)
            set ("units:" ++ id ++ ":token") token
            set ("units:" ++ id ++ ":location") (l2b $ encode p)

            -- you don't exist until you step off the golden square
            --hset "world" (l2b $ encode p) id

            -- hearbeat
            heartbeat id
            sadd "units" [id]

            -- they need to know the info, the token, and the starting location, etc
            return $ Right $ Spawn id token p

unitMove :: ByteString -> Point -> Redis (Either Fault String)
unitMove uid p = do

    -- record heartbeat
    heartbeat uid

    let lk = ("units:" ++ uid ++ ":location")
    ep <- get lk
    case ep of
        Left r -> return $ Left $ Fault (pack $ show r)
        Right Nothing -> return $ Left $ Fault "Could not find location"
        Right (Just bs) -> do

            -- possible error, except we put in ourselves
            let po = fromJust $ decode $ b2l bs

            if (not $ (neighboring p po && validPoint worldInfo p)) then 
                return $ Left $ Fault "Invalid move"
            else do

            res <- claimLocation uid p
            case res of 
                Right True -> do
                    set lk (l2b $ encode p)
                    hdel "world" [l2b $ encode po]
                    return $ Right "OK"
                Left f -> return $ Left f


claimLocation :: ByteString -> Point -> Redis (Either Fault Bool)
claimLocation uid p = do
    res <- hsetnx "world" (l2b $ encode p) uid
    case res of 
        Right True -> return $ Right True
        _ -> return $ Left $ Fault "Space Occupied"

neighboring :: Point -> Point -> Bool
neighboring p1 p2 = withinOne (x p1) (x p2) && withinOne (y p1) (y p2)
    where withinOne a b = (a == b || a == (b-1) || a == (b+1))

validPoint :: FieldInfo -> Point -> Bool
validPoint f p = xmin <= px && px <= xmax && ymin <= py && py <= ymax
    where px = x p
          py = y p
          xmin = x $ fieldStart f
          xmax = xmin + (width $ fieldSize f)
          ymin = y $ fieldStart f
          ymax = ymin + (height $ fieldSize f)

resetWorld :: Redis ()
resetWorld = do
    flushall
    return ()

authorized :: ByteString -> ByteString -> Redis Bool
authorized uid token = do
    r <- get ("units:" ++ uid ++ ":token")
    case r of
        Right (Just bs) -> return (token == bs)
        _ -> return False





-- inactive units
-- I like the other guy's solution. diff all the users against the active users
-- active users = union of all recent-enough heartbeat sets
-- and I can automatically clean up the old heartbeats
heartbeat :: ByteString -> Redis ()
heartbeat id = do
    dt <- liftIO $ getCurrentTime
    let ds = dateString dt
    set (unitHeartbeatKey id) ds
    return ()

removeInactiveUnits :: DateTime -> Redis ()
removeInactiveUnits dt = do 
    let ds = dateString dt
    res <- smembers "units"
    case res of 
        Left _ -> return ()
        Right ids -> do
            res <- mget (map unitHeartbeatKey ids)
            case res of
                Left _ -> return ()
                Right mbeats -> do
                    let oldIds = map fst $ filter (oldHeartBeat ds) $ zip ids mbeats
                    mapM_ removeUnit oldIds
                    return ()

dateString :: DateTime -> ByteString
dateString = pack . (formatDateTime "%Y-%m-%d %H:%M")

removeUnit :: ByteString -> Redis ()
removeUnit id = do 
    res <- get (unitLocationKey id)
    case res of 
        Left _ -> return ()
        Right Nothing -> return ()
        Right (Just loc) -> do
            hdel "world" [loc]
            del (unitKeys id)
            srem "units" [id]
            return ()

unitHeartbeatKey id = "units:" ++ id ++ ":heartbeat"
unitLocationKey id = "units:" ++ id ++ ":location"
unitDescriptionKey id = "units:" ++ id ++ ":description"
unitTokenKey id = "units:" ++ id ++ ":token"
unitKeys id = [unitHeartbeatKey id, unitLocationKey id, unitDescriptionKey id, unitTokenKey id]

-- these are ALWAYS in order, I guess.
oldHeartBeat :: ByteString -> (ByteString, Maybe ByteString) -> Bool
oldHeartBeat date (id, mhb) = case mhb of
        Nothing -> False
        Just hb -> (hb < date)




