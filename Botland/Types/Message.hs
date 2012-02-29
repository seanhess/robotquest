{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Botland.Types.Message where

import Data.ByteString.Char8 (ByteString)
import Data.Data (Data, Typeable)

-- message you might want to return.
data Fault = Fault { message :: ByteString } 
           | NotFound
           | NotAuthorized
           deriving (Data, Typeable)

