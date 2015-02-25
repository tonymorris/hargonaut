{-# LANGUAGE DeriveDataTypeable #-}

module Data.Json.Json where

import Data.Bool(Bool)
import Data.Data(Data)
import Data.Eq(Eq)
import Data.Map(Map)
import Data.Ord(Ord)
import Data.Typeable(Typeable)

import Prelude(Show)

-- todo parameterise?
type JsonMap =
  Map

-- todo parameterise?
type JsonArray =
  []

data Json s n =
  Object (JsonMap s (Json s n))
  | Array (JsonArray (Json s n))
  | String s
  | Number n
  | Boolean Bool
  | Null
  deriving (Eq, Ord, Show, Data, Typeable)
