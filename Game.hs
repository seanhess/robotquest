{-# LANGUAGE OverloadedStrings #-}

import Botland.Tick
import Botland.Helpers
import Botland.Control

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)

import Database.MongoDB (runIOE, connect, access, master, host, Pipe, Action)

cleanupDelay :: Integer
cleanupDelay = 2 

main = do
    pipe <- connectMongo
    let db action = liftIO $ access pipe master "botland" action
    db ensureIndexes

    -- run cleanup every so often
    let cleanup = do 
        putStrLn "CLEANUP"
        db $ cleanupInactives cleanupDelay
        threadDelay ((fromIntegral cleanupDelay)*1000000)
        cleanup

    -- cleanup every once in awhile, not as often as the game tick 
    forkIO $ cleanup

    runTick game db
    return ()
