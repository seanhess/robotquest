{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleInstances, DeriveGeneric #-}

module Botland.Types where

import Control.Applicative ((<$>))

import Data.Data (Data, Typeable, typeOf)
import Data.Aeson (ToJSON(..), (.=), object, (.:), FromJSON, Object(..))

data Unit = Unit { x :: Int, y :: Int } deriving (Show, Data, Typeable)
data Error = Error { error :: String } deriving (Data, Typeable)

-- newtype Error = Error { errMessage :: String } deriving (Show, Data, Typeable)
-- instance ToJSON Error where
--     toJSON s = object ["error" .= errMessage s]


