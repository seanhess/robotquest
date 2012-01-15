{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Botland.Types where

import Data.Data (Data, Typeable, typeOf)
import Data.Aeson (ToJSON(..), (.=), object)
import qualified Data.Aeson.Generic as AG

import qualified Data.Text as T
import Data.Char (toLower)

data Unit = Unit { x :: Int, y :: Int } deriving (Show, Data, Typeable)


data Message a = Message { obj :: a } |
                 -- Token { obj :: a , token :: String }
                 Error String 
                 deriving (Show, Data, Typeable)


instance (Data a, Typeable a) => ToJSON (Message a) where
    toJSON m = object [typeName .= AG.toJSON o]
        where typeName = T.pack $ map toLower $ show $ typeOf o
              o = obj m



