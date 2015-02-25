{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Json.Json
(
  JsonObject
, AsObject(_Object)
, JsonArray
, AsArray(_Array)
, JsonString
, AsString(_String)
, JsonNumber
, AsNumber(_Number)
, Json(Object, Array, String, Number, Boolean, Null)
, AsJson(_Json)
, AsBoolean(_Boolean)
, AsNull(_Null)
, isTrue
, true
, isFalse
, false
, isNull
, null
, isZero
, zero
, isOne
, one
, isEmptyString
, emptyString
, isSingleString
, singleString
, isEmptyArray
, emptyArray
, isSingleArray
, singleArray
, isEmptyObject
, emptyObject
, isSingleObject
, singleObject
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id, (.)))
import Control.Lens(Choice, Optic', (#), (^?), prism', has, _tail)
import Data.Bool(Bool(False, True), not)
import Data.Char(Char)
import Data.Data(Data)
import Data.Eq(Eq((==)))
import Data.Foldable(any)
import qualified Data.List as List(null)
import Data.Map(Map)
import qualified Data.Map as Map(empty, null, size, singleton)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Scientific(Scientific)
import Data.Text(Text)
import qualified Data.Text as Text(null, empty, singleton)
import Data.Typeable(Typeable)
import Prelude(Show)

-- todo parameterise?
-- issue 1
type JsonObject =
  Map JsonString Json

class AsObject p f s where
  _Object ::
    Optic' p f s JsonObject
 
instance AsObject p f JsonObject where
  _Object =
    id

-- todo parameterise?
-- issue 1
type JsonArray =
  [Json]

class AsArray p f s where
  _Array ::
    Optic' p f s JsonArray
 
instance AsArray p f JsonArray where
  _Array =
    id

-- todo parameterise?
-- issue 1
type JsonString =
  Text

class AsString p f s where
  _String ::
    Optic' p f s JsonString
 
instance AsString p f JsonString where
  _String =
    id

-- todo parameterise?
-- issue 1
type JsonNumber =
  Scientific

class AsNumber p f s where
  _Number ::
    Optic' p f s JsonNumber
 
instance AsNumber p f JsonNumber where
  _Number =
    id

data Json =
  Object JsonObject
  | Array JsonArray
  | String JsonString
  | Number JsonNumber
  | Boolean Bool
  | Null
  deriving (Eq, Ord, Show, Data, Typeable)

class AsJson p f s where
  _Json ::
    Optic' p f s Json
 
class AsBoolean p f s where
  _Boolean ::
    Optic' p f s Bool
 
class AsNull p f s where
  _Null ::
    Optic' p f s ()
 
instance AsJson p f Json where
  _Json =
    id

instance (Choice p, Applicative f) => AsObject p f Json where
  _Object =
    prism'
      Object
      (\case
          Object m ->
            Just m
          _ ->
            Nothing
      )

instance (Choice p, Applicative f) => AsArray p f Json where
  _Array =
    prism'
      Array
      (\case
          Array a ->
            Just a
          _ ->
            Nothing
      )

instance (Choice p, Applicative f) => AsString p f Json where
  _String =
    prism'
      String
      (\case
          String s ->
            Just s
          _ ->
            Nothing
      )

instance (Choice p, Applicative f) => AsNumber p f Json where
  _Number =
    prism'
      Number
      (\case
          Number n ->
            Just n
          _ ->
            Nothing
      )

instance (Choice p, Applicative f) => AsBoolean p f Json where
  _Boolean =
    prism'
      Boolean
      (\case
          Boolean b ->
            Just b
          _ ->
            Nothing
      )

instance (Choice p, Applicative f) => AsNull p f Json where
  _Null =
    prism'
      (\() -> Null)
      (\case
          Null ->
            Just ()
          _ ->
            Nothing
      )

isTrue ::
  Json
  -> Bool
isTrue j =
  any id (j ^? _Boolean)

true ::
  Json
true =
  _Boolean # True

isFalse ::
  Json
  -> Bool
isFalse j =
  any not (j ^? _Boolean)

false ::
  Json
false =
  _Boolean # False

isNull ::
  Json
  -> Bool
isNull =
  has _Null

null ::
  Json
null =
  _Null # ()

isZero ::
  Json
  -> Bool
isZero j =
  any (== 0) (j ^? _Number)

zero ::
  Json
zero =
  _Number # 0

isOne ::
  Json
  -> Bool
isOne j =
  any (== 1) (j ^? _Number)

one ::
  Json
one =
  _Number # 1

isEmptyString ::
  Json
  -> Bool
isEmptyString j =
  any Text.null (j ^? _String)

emptyString ::
  Json
emptyString =
  _String # Text.empty

isSingleString ::
  Json
  -> Bool
isSingleString j =
  any Text.null (j ^? _String . _tail)

singleString ::
  Char
  -> Json
singleString c =
  _String # Text.singleton c

isEmptyArray ::
  Json
  -> Bool
isEmptyArray j =
  any List.null (j ^? _Array)

emptyArray ::
  Json
emptyArray =
  _Array # []

isSingleArray ::
  Json
  -> Bool
isSingleArray j =
  any List.null (j ^? _Array . _tail)

singleArray ::
  Json
  -> Json
singleArray e =
  _Array # [e]

isEmptyObject ::
  Json
  -> Bool
isEmptyObject j =
  any Map.null (j ^? _Object)

emptyObject ::
  Json
emptyObject =
  _Object # Map.empty

isSingleObject ::
  Json
  -> Bool
isSingleObject j =
  any (\m -> Map.size m == 1) (j ^? _Object)

singleObject ::
  JsonString
  -> Json
  -> Json
singleObject k v =
  _Object # Map.singleton k v
