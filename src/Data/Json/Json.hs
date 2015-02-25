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
, AsTrue(_True)
, AsFalse(_False)
, AsZero(_Zero)
, AsOne(_One)
, AsEmptyString(_EmptyString)
, AsSingleString(_SingleString)
, AsEmptyArray(_EmptyArray)
, AsSingleArray(_SingleArray)
, AsEmptyObject(_EmptyObject)
, AsSingleObject(_SingleObject)
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id, (.)))
import Control.Lens(Choice, Optic', _Cons, (#), (^?), prism', _head)
import Control.Monad((=<<))
import Data.Bool(Bool(False, True), not, bool)
import Data.Char(Char)
import Data.Data(Data)
import Data.Eq(Eq((==)))
import Data.Foldable(any)
import qualified Data.List as List(null)
import Data.Map(Map)
import qualified Data.Map as Map(empty, null, size, singleton, toList)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.Scientific(Scientific)
import Data.Text(Text)
import qualified Data.Text as Text(null, empty, singleton)
import Data.Typeable(Typeable)
import Prelude(Show)

-- $setup
-- >>> import Data.Functor((<$>))
-- >>> import Data.Scientific(scientific)
-- >>> import Test.QuickCheck(Arbitrary(arbitrary), Gen, oneof)
-- >>> import Test.QuickCheck.Instances()
-- >>> import Control.Applicative(Applicative(pure))
-- >>> :{
--instance Arbitrary Json where
--  arbitrary =
--    genJson
--genJsonNumber ::
--  Gen JsonNumber
--genJsonNumber =
--  do c <- arbitrary
--     b <- arbitrary
--     pure (scientific c b)
--genJson ::
--  Gen Json
--genJson =
--  oneof
--    [
--      Object <$> arbitrary
--    , Array <$> arbitrary
--    , String <$> arbitrary
--    , Number <$> genJsonNumber
--    , Boolean <$> arbitrary
--    , pure Null
--    ]
-- :}

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

boolMaybe ::
  Bool
  -> Maybe ()
boolMaybe =
  bool Nothing (Just ())

class AsTrue p f s where
  _True ::
    Optic' p f s ()
    
instance (Choice p, Applicative f) => AsTrue p f Json where
  _True =
    prism'
      (\() -> _Boolean # True)
      (\j -> boolMaybe (any id (j ^? _Boolean)))

class AsFalse p f s where
  _False ::
    Optic' p f s ()
    
instance (Choice p, Applicative f) => AsFalse p f Json where
  _False =
    prism'
      (\() -> _Boolean # False)
      (\j -> boolMaybe (any not (j ^? _Boolean)))

class AsZero p f s where
  _Zero ::
    Optic' p f s ()
    
instance (Choice p, Applicative f) => AsZero p f Json where
  _Zero =
    prism'
      (\() -> _Number # 0)
      (\j -> boolMaybe (any (== 0) (j ^? _Number)))

class AsOne p f s where
  _One ::
    Optic' p f s ()
    
instance (Choice p, Applicative f) => AsOne p f Json where
  _One =
    prism'
      (\() -> _Number # 1)
      (\j -> boolMaybe (any (== 1) (j ^? _Number)))

class AsEmptyString p f s where
  _EmptyString ::
    Optic' p f s ()
    
instance (Choice p, Applicative f) => AsEmptyString p f Json where
  _EmptyString =
    prism'
      (\() -> _String # Text.empty)
      (\j -> boolMaybe (any Text.null (j ^? _String)))

class AsSingleString p f s where
  _SingleString ::
    Optic' p f s Char
    
instance (Choice p, Applicative f) => AsSingleString p f Json where
  _SingleString =
    prism'
      (\c -> _String # Text.singleton c)
      (\j -> (\(h, t) -> bool Nothing (Just h) (Text.null t)) =<< j ^? _String . _Cons)

class AsEmptyArray p f s where
  _EmptyArray ::
    Optic' p f s ()
    
instance (Choice p, Applicative f) => AsEmptyArray p f Json where
  _EmptyArray =
    prism'
      (\() -> _Array # [])
      (\j -> boolMaybe (any List.null (j ^? _Array)))

class AsSingleArray p f s where
  _SingleArray ::
    Optic' p f s Json
    
instance (Choice p, Applicative f) => AsSingleArray p f Json where
  _SingleArray =
    prism'
      (\e -> _Array # [e])
      (\j -> (\(h, t) -> bool Nothing (Just h) (List.null t)) =<< j ^? _Array . _Cons)

class AsEmptyObject p f s where
  _EmptyObject ::
    Optic' p f s ()
    
instance (Choice p, Applicative f) => AsEmptyObject p f Json where
  _EmptyObject =
    prism'
      (\() -> _Object # Map.empty)
      (\j -> boolMaybe (any Map.null (j ^? _Object)))

class AsSingleObject p f s where
  _SingleObject ::
    Optic' p f s (JsonString, Json)
    
instance (Choice p, Applicative f) => AsSingleObject p f Json where
  _SingleObject =
    prism'
      (\(k, v) -> _Object # Map.singleton k v)
      (\j -> (\m -> bool Nothing (Map.toList m ^? _head) (Map.size m == 1)) =<< j ^? _Object)
