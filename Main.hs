{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)

import Happstack.Ella (Req, body, cap, route, get, post, mount)
import Happstack.Lite

import Happstack.Server (takeRequestBody, askRq)
import Happstack.Server.Types (RqBody(..))

import Data.Data (Data)

import Botland.Types

import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = serve Nothing app

app :: ServerPart Response
app = route $ do
    get "/unit/:unitId" unitDetails
    post "/creature/create" createCreature
    mount $ serveDirectory DisableBrowsing ["index.html"] "./public" 


unitDetails :: Req -> ServerPart Response
unitDetails r = do
    let unitId = cap r
    ok $ toResponse $ encode (Error ("not implemented, but you gave us the id: " ++ unitId))
    

-- screw it, just use headers for the token
-- I need to generate a unique id for the unit
-- now set a header
createCreature :: Req -> ServerPart Response
createCreature r = do
    let creature :: Maybe Creature = decode (body r)
    liftIO $ print creature
    
    case creature of 
        Nothing -> internalServerError $ toResponse $ encode $ Error "Could not parse your creature"
        Just c -> do
            let unitId = "id"
                unitToken = "abcdefg"
            setHeaderM "X-Unit-Token" unitToken
            ok $ toResponse $ encode $ CreatureUnit unitId c
    

    -- pretend I generate these

{-

CREATE A UNIT
[√] POST /unit/create
[ ] Creates it in the data store
[ ] Figure out data storage (redis, mongo?)
[ ] Create a unit id
[ ] Create a unit control token
[√] Send down the unit (body)
[ ] Send down control token (header? extra body field?)

[ ] Protocol: {unitToken: "", unit: {}}, rather than headers, because headers are hard for javascript. 
[ ] {error: "", unit{}}, etc, rather than simply body messages. It's just easier to deal with.


THINGS TO EXPERIMENT WITH
[ ] Figure out redis
[ ] Proper way to send down that token along with the message

-}


