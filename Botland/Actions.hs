{-# LANGUAGE OverloadedStrings #-}

module Botland.Actions where

import Prelude hiding ((++))
import qualified Prelude

import Botland.Helpers (uuid, l2b, b2l, (++))
import Botland.Types.Message (Fault(..))
import Botland.Types.Location (Field(..), Point(..), Size(..), Location(..), FieldInfo(..))
import Botland.Types.Unit (Spawn(..), UnitDescription(..), SpawnRequest(..))

import Control.Monad.IO.Class (liftIO)

import Database.Redis hiding (decode)

import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.ByteString.Char8 (pack, unpack, concat, ByteString(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.DateTime (getCurrentTime, formatDateTime, DateTime)
import qualified Data.Text.Lazy as T
import Data.Maybe (fromMaybe, fromJust, isNothing, isJust)

import Debug.Trace (trace)



-- the field for the whole world. Remove this eventually in favor of smaller fields
worldLocations :: Redis ([Location])
worldLocations = do
    r <- smembers "minions"  
    let ids = result r
    let keys = map locationKey ids
    r <- mget keys
    let kp = zip ids $ result r
    return $ (map toLocation kp)

toLocation :: (ByteString, Maybe ByteString) -> Location
toLocation (id, Nothing) = error "Could not find point"
toLocation (id, mps) = Location p id
    where p = fromMaybe (Point 0 0) $ decode $ b2l (fromJust mps)

unitGetDescription :: ByteString -> Redis UnitDescription
unitGetDescription id = do
    res <- get ("minions:" ++ id ++ ":description")
    return $ parse $ fromJust $ result res


-- WARNING: This will throw an error, but it's only if redis has an error, which
-- is unlikely. It's also unlikely we want the service to really run if that's the
-- case anyway. 
result :: Either Reply a -> a
result era = case era of 
    Right a -> a
    _ -> error "redis error" 

-- WARNING: this will throw an error, so only use this when you put the object there. 
parse :: (FromJSON a) => ByteString -> a
parse bs = case (decode $ b2l bs) of
    Just a -> a
    Nothing -> error "Could not parse object"

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

    open <- claimLocation id p

    if (not open) then
        return $ Left $ Fault "Space occupied"
    else do

    -- save the actor information, its token, and its position
    set (descriptionKey id) (stringify d)
    set (tokenKey id) token
    set (locationKey id) (stringify p)

    -- hearbeat
    sadd "minions" [id]

    -- they need to know the info, the token, and the starting location, etc
    return $ Right $ Spawn id token p


-- returns True if you successfully grabbed it
claimLocation :: ByteString -> Point -> Redis Bool
claimLocation id p = do
    r <- setnx (worldLocationKey p) id
    return $ result r



unitMove :: FieldInfo -> ByteString -> Point -> Redis (Either Fault ())
unitMove worldInfo id p = do
    r <- get (locationKey id)
    let po = parse $ fromJust $ result r

    if (not $ (neighboring p po && validPoint worldInfo p)) then 
        return $ Left $ Fault "Invalid move"
    else do

    open <- claimLocation id p

    if (not open) then
        return $ Left $ Fault "Space occupied"
    else do

    --liftIO $ putStrLn "MOVING" 
    --liftIO $ print p
    --liftIO $ print id

    set (locationKey id) (stringify p)
    del [(worldLocationKey p)]
    return $ Right () 


--unitAttack :: FieldInfo -> ByteString -> Point -> Redis (Either Fault Bool)
--unitAttack worldInfo uid p = do

--    let lk = ("units:" ++ uid ++ ":location")
--    ep <- get lk
--    case ep of
--        Left r -> return $ Left $ Fault (pack $ show r)
--        Right Nothing -> return $ Left $ Fault "Could not find location"
--        Right (Just bs) -> do

--            -- possible error, except we put in ourselves
--            let po = fromJust $ decode $ b2l bs

--            if (not $ (neighboring p po && validPoint worldInfo p)) then 
--                return $ Left $ Fault "Invalid attack"
--            else do

--            -- conditional unset of the world
--            -- if it succeeds, then clean up the unit
--            -- wait, this kills YOU
--            res <- hget "world" (l2b $ encode p)
--            case res of 
--                Right (Just uid) -> do
--                    res <- removeUnit uid
--                    case res of
--                        Left f -> return $ Left f                    
--                        Right _ -> return $ Right True
--                _ -> return $ Left $ Fault "Invalid attack"




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
    r <- get (tokenKey uid)
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


--removeUnit :: ByteString -> Redis (Either Fault ())
--removeUnit id = do 

--    -- if they move after this, I can't remove them!
--    res <- get (locationKey id)
--    case res of 
--        Left _ -> return $ Left $ Fault "Could not remove unit"
--        Right Nothing -> return $ Left $ Fault "Could not find unit location"
--        Right (Just loc) -> do
--            res <- hdel "world" [loc]
--            case res of 
--                Right _ -> do 
--                    del (unitKeys id)
--                    srem "units" [id]
--                    return $ Right ()
--                _ -> return $ Left $ Fault "Could not remove from map"


locationKey id = "minions:" ++ id ++ ":location"
descriptionKey id = "minions:" ++ id ++ ":description"
tokenKey id = "minions:" ++ id ++ ":token"
minionKeys id = [locationKey id, descriptionKey id, tokenKey id]
worldLocationKey p = ("world:locations:" ++ (pointKey p)) 

pointKey :: Point -> ByteString
pointKey p = pack $ (show (x p) Prelude.++ "," Prelude.++ show (y p))

stringify :: (ToJSON a) => a -> ByteString
stringify a = l2b $ encode a

