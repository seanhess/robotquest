{-# LANGUAGE OverloadedStrings #-}

module Botland.Actions where

import Prelude hiding ((++))

import Botland.Helpers (uuid, l2b, b2l, (++))
import Botland.Types.Message (Fault(..))
import Botland.Types.Location (Field(..), Point(..), Size(..), Location(..), FieldInfo(..))
import Botland.Types.Unit (Spawn(..), UnitDescription(..), SpawnRequest(..))

import Control.Monad.IO.Class (liftIO)

import Database.Redis hiding (decode)

import Data.Aeson (encode, decode)
import Data.ByteString.Char8 (pack, unpack, concat, ByteString(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.DateTime (getCurrentTime, formatDateTime, DateTime)
import qualified Data.Text.Lazy as T
import Data.Maybe (fromMaybe, fromJust)

import Debug.Trace (trace)



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

unitSpawn :: FieldInfo -> SpawnRequest -> Redis (Either Fault Spawn)
unitSpawn worldInfo sr = do
    id <- uniqueId
    token <- uniqueId

    -- everyone starts at the same place. You don't exist until you move off of it.
    -- create a spawnAt 
    let d = unitDescription sr 
    let p = requestedPoint sr

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
            sadd "units" [id]

            -- they need to know the info, the token, and the starting location, etc
            return $ Right $ Spawn id token p

unitMove :: FieldInfo -> ByteString -> Point -> Redis (Either Fault String)
unitMove worldInfo uid p = do

    let lk = ("units:" ++ uid ++ ":location")
    ep <- get lk
    case ep of
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
        _ -> return $ Left $ Fault "Could not find location"


unitAttack :: FieldInfo -> ByteString -> Point -> Redis (Either Fault Bool)
unitAttack worldInfo uid p = do

    let lk = ("units:" ++ uid ++ ":location")
    ep <- get lk
    case ep of
        Left r -> return $ Left $ Fault (pack $ show r)
        Right Nothing -> return $ Left $ Fault "Could not find location"
        Right (Just bs) -> do

            -- possible error, except we put in ourselves
            let po = fromJust $ decode $ b2l bs

            if (not $ (neighboring p po && validPoint worldInfo p)) then 
                return $ Left $ Fault "Invalid attack"
            else do

            -- conditional unset of the world
            -- if it succeeds, then clean up the unit
            -- wait, this kills YOU
            res <- hget "world" (l2b $ encode p)
            case res of 
                Right (Just uid) -> do
                    res <- removeUnit uid
                    case res of
                        Left f -> return $ Left f                    
                        Right _ -> return $ Right True
                _ -> return $ Left $ Fault "Invalid attack"



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
validPoint f p = xmin <= px && px < xmax && ymin <= py && py < ymax
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


-- throw an error on failure. means the db is down anyway
uniqueId :: Redis ByteString
uniqueId = do
    res <- incr "id"
    case res of 
        Right i -> return $  pack $ show i 
        _ -> error "Could not get id"


dateString :: DateTime -> ByteString
dateString = pack . (formatDateTime "%Y-%m-%d %H:%M")

removeUnit :: ByteString -> Redis (Either Fault ())
removeUnit id = do 

    -- if they move after this, I can't remove them!
    res <- get (unitLocationKey id)
    case res of 
        Left _ -> return $ Left $ Fault "Could not remove unit"
        Right Nothing -> return $ Left $ Fault "Could not find unit location"
        Right (Just loc) -> do
            res <- hdel "world" [loc]
            case res of 
                Right _ -> do 
                    del (unitKeys id)
                    srem "units" [id]
                    return $ Right ()
                _ -> return $ Left $ Fault "Could not remove from map"


unitLocationKey id = "units:" ++ id ++ ":location"
unitDescriptionKey id = "units:" ++ id ++ ":description"
unitTokenKey id = "units:" ++ id ++ ":token"
unitKeys id = [unitLocationKey id, unitDescriptionKey id, unitTokenKey id]





