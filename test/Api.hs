{-# LANGUAGE OverloadedStrings #-}

-- https://gist.github.com/967505

-- Rely on Errors
-- Assume you start things by hand (the server, the database, etc)

import Test.QuickCheck (Property, quickCheck, (==>), quickCheckWith, stdArgs, maxSuccess) 
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.Property (Testable)

import Network.HTTP (simpleHTTP, Request(..), Response)
import Network.HTTP.Base (mkRequest, RequestMethod(GET, POST, DELETE, PUT), rspBody, rqBody, rqHeaders)
import Network.HTTP.Headers (insertHeader, mkHeader, HeaderName(..))
import Network.URI    (parseURI)

import Data.Aeson (FromJSON, encode, decode, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromJust)

import Botland.Types 

main = do 
    putStrLn "Running Tests..."
    --functional testAuthentication

whatever :: IO ()
whatever = do
    putStrLn "whatever"

-- the only thing I really need to test functionally is that the token/auth stuff works
-- everything else, just use the redis functions directly? 
--world :: IO String 
--world = do
--    res <- send (get "http://localhost:3000/world")
--    let mw = decode $ L.pack (rspBody res) :: Maybe Field 
--    print (rspBody res)
--    return $ rspBody res

--postNewActor :: IO (Unit Actor)
--postNewActor = do
--    let actor = Actor Bot 
--    res <- send (request POST "http://localhost:3000/actor/new" actor)
--    let body = rspBody res
--        mua = decode body :: Unit Actor
--    case decode body of
--        Nothing -> error ("Could not parse " ++ (L.unpack body))
--        Just ua -> return ua

--moveActorWithToken :: IO ()
--moveActorWithToken = undefined

--moveActorWithoutToken :: IO ()
--moveActorWithoutToken = undefined

--testAuthentication :: Property
--testAuthentication = monadicIO $ do
--    ua <- run $ postNewActor
--    assert $ True


-- then, test the redis stuff on its own


















-- Helpers


functional :: (Testable prop) => prop -> IO ()
functional = quickCheckWith stdArgs { maxSuccess = 5 } 

get :: String -> Request L.ByteString
get u = request GET u Empty

post :: (ToJSON a) => String -> a -> Request L.ByteString
post u b = request POST u b 

request :: (ToJSON a) => RequestMethod -> String -> a -> Request L.ByteString
request rm u b = case parseURI u of
    Nothing -> error ("Bad URL" ++ u)
    Just url -> Request url POST headers body
        where body = encode b
              headers = [ mkHeader HdrContentType "application/json"
                        , mkHeader HdrContentLength (show (L.length body))
                        ]

send :: Request L.ByteString -> IO (Response L.ByteString)
send req = do
    result <- simpleHTTP req
    return $ case result of 
        Left e -> error $ show e 
        Right r -> r



