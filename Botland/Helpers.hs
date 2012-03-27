{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}

module Botland.Helpers where

import Botland.Types hiding (Direction(..))

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)

import Database.MongoDB (Failure(..))

import Data.Aeson (decode, ToJSON, FromJSON, encode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString, pack, unpack, append)
import Data.Conduit.Lazy (lazyConsume)
import qualified Data.Text.Lazy as T

import Network.HTTP.Types (status404, status500, status400, status401, status200, status403, status501)
import Network.Wai (requestBody)

import Web.Scotty (ActionM, request, raise, status, text, redirect, rescue, header, json, param)
import qualified Web.Scotty

-- pull out the body as a lazy bytestring (easy for Data.Aeson to consume)
body :: ActionM (L.ByteString)
body = do
    r <- request
    bss <- liftIO . runResourceT . lazyConsume . requestBody $ r
    return $ L.fromChunks bss

-- converts a lazy bytestring to strict bytestring
l2b :: L.ByteString -> B.ByteString
l2b = B.concat . L.toChunks

b2l :: B.ByteString -> L.ByteString
b2l l = L.fromChunks [l]

b2t :: B.ByteString -> T.Text
b2t = T.pack . unpack 



--class Sendable a where
--    send :: a -> ActionM ()

--class Woot where
--    woot :: String

--instance (ToJSON a) Response a where
--instance Response Fault where

-- handles the fault checking, sending proper stuff

--instance (Sendable a) => Sendable (Either Failure a) where
--    send m ea

--instance (ToJSON a) => Sendable a where
--    send a = do
--        status status200
--        json a


-- I want the method to work on all sorts of different stuff
-- bah, lame-pants
-- well, wait, all I have to do is CONVERT the different types of things to other things
-- I could write a bunch of functions to do that
-- unique functions, with unique names.

-- just make different functions for the different things, and don't call them "send"


sendAction :: (ToJSON a) => String -> Either Failure a -> ActionM ()
sendAction _ (Left (DocNotFound _)) = do
    status status404
sendAction m (Left (QueryFailure _ _)) = do
    status status400
    json $ Fault m
sendAction m (Left (WriteFailure _ _)) = do
    status status400
    json $ Fault m
sendAction _ (Left _) = do
    status status500
    json $ Fault "Database Error"
sendAction _ (Right a) = json a

sendActionFault :: (ToJSON a) => String -> Either Failure (Either Fault a) -> ActionM()
sendActionFault m (Right efa) = case efa of 
    Left f -> fault f 
    Right a -> json a
sendActionFault m f = sendAction m f


-- send a non-mongo fault
fault :: Fault -> ActionM ()
fault f = do
    case f of 
        NotAuthorized -> status status403
        NotFound -> status status400
        InvalidPosition -> status status400
        NotImplemented -> status status501
        _ -> status status500
    json f







