{-# LANGUAGE OverloadedStrings #-}

module Main where

import Botland.Actions (removeInactiveUnits)

import Database.Redis (runRedis, connect, defaultConnectInfo)
import Data.ByteString.Char8 (pack, unpack, append, concat)
import Data.Maybe (fromMaybe)

import Data.DateTime (getCurrentTime)

import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    db <- connect defaultConnectInfo
    dt <- getCurrentTime
    runRedis db (removeInactiveUnits dt)
    putStrLn "Inactive Units Before"
    print dt
