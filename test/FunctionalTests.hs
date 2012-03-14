{-# LANGUAGE OverloadedStrings #-}

-- https://gist.github.com/967505

-- Rely on Errors
-- Assume you start things by hand (the server, the database, etc)

import Test.QuickCheck (Property, quickCheck, (==>), quickCheckWith, stdArgs, maxSuccess) 
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.Property (Testable)

import Network.HTTP (simpleHTTP, Request(..), Response)
import Network.HTTP.Base (mkRequest, RequestMethod(GET, POST, DELETE, PUT), rspBody, rqBody, rqHeaders)
import Network.HTTP.Headers (replaceHeader, mkHeader, HeaderName(..))
import Network.URI    (parseURI)

import Data.Aeson (FromJSON, encode, decode, ToJSON)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromJust)

import Botland.Types.Unit (Spawn(..), UnitDescription(..), SpawnRequest(..))
import Botland.Types.Message (Fault(..), Empty(..))
import Botland.Types.Location (Point(..))

main = do 
    putStrLn "Running Tests..."
    functional testAuthentication

-- the only thing I really need to test functionally is that the token/auth stuff works
-- everything else, just use the redis functions directly? 
--world :: IO String 
--world = do
--    res <- send (get "http://localhost:3000/world")
--    let mw = decode $ L.pack (rspBody res) :: Maybe Field 
--    print (rspBody res)
--    return $ rspBody res

clear :: IO () 
clear = do
    send (request POST "http://localhost:3000/admin/clear")
    return ()

postNewUnit :: IO (Spawn)
postNewUnit = do
    let desc = UnitDescription "source" "notes"
    let p = Point 50 50
    let sr = SpawnRequest p desc
    let req = setBody sr (request POST "http://localhost:3000/units")
    res <- send req
    let body = rspBody res
    case (decode body :: Maybe Spawn) of
        Nothing -> error ("Could not parse " ++ (L.unpack body) ++ " as Spawn")
        Just u -> do
            return u

moveActorWithoutToken :: ByteString -> Point -> IO (Fault)
moveActorWithoutToken uid p = do
    let req = setBody p (request POST ("http://localhost:3000/units/" ++ (unpack uid) ++ "/move"))
    res <- send req
    let body = rspBody res
    case (decode body :: Maybe Fault) of
        Nothing -> error ("Was not a fault: " ++ (L.unpack body))
        Just f -> return f 

moveActorWithToken :: ByteString -> ByteString -> Point -> IO Bool
moveActorWithToken uid token p = do
    let req = setHeader "X-Auth-Token" (unpack token) $ setBody p (request POST ("http://localhost:3000/units/" ++ (unpack uid) ++ "/move")) 
    res <- send req
    let body = rspBody res
    case (decode body :: Maybe Fault) of
        Just f -> return $ error (show f)
        Nothing -> return True


removeUnit :: ByteString -> IO ()
removeUnit uid = do
    let req = request DELETE ("http://localhost:3000/units/" ++ (unpack uid))
    res <- send req
    return ()

testAuthentication :: Property
testAuthentication = monadicIO $ do

    run $ clear

    u <- run $ postNewUnit
    let uid = unitId u
        token = unitToken u
        p = (spawnPoint u) { x = 51 }

    -- running it twice should fail with "Space Occupied"
    --run $ postNewUnit

    -- will already fail if it isn't a fault
    f <- run $ moveActorWithoutToken uid p
    r <- run $ moveActorWithToken uid token p

    res <- run $ removeUnit uid

    --assert r
    return ()


-- then, test the redis stuff on its own
















-- Helpers


functional :: (Testable prop) => prop -> IO ()
functional = quickCheckWith stdArgs { maxSuccess = 1 } 

--get :: String -> Request L.ByteString
--get u = request GET u Empty

--post :: (ToJSON a) => String -> a -> Request L.ByteString
--post u b = request POST u b 

request :: RequestMethod -> String -> Request L.ByteString
request rm u = case parseURI u of
    Nothing -> error ("Bad URL" ++ u)
    Just url -> mkRequest rm url

send :: Request L.ByteString -> IO (Response L.ByteString)
send req = do
    result <- simpleHTTP req
    return $ case result of 
        Left e -> error $ show e 
        Right r -> r


setBody :: (ToJSON a) => a -> Request L.ByteString -> Request L.ByteString
setBody obj req = req' { rqBody = body }
  where
    body = encode obj
    req' = replaceHeader HdrContentType "application/json" .
           replaceHeader HdrContentLength (show $ L.length body) $
           req


setHeader :: String -> String -> Request L.ByteString -> Request L.ByteString  
setHeader name value = replaceHeader (HdrCustom name) value


