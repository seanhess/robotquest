{-# LANGUAGE OverloadedStrings #-}

import Botland.Tick
import Botland.Helpers
import Botland.Control
import Botland.Types

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)

import Database.MongoDB (runIOE, connect, access, master, host, Pipe, Action)

main = do
    pipe <- connectMongo
    let db action = liftIO $ access pipe master "botland" action
    db ensureIndexes

    forkIO $ cleanup db
    runTick gameInfo db
    return ()
