{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Data.Aeson.Lens (
  ValueIx(..),
  valueAt,
  arr, obj,
  ) where

import Data.Aeson
import Control.Lens
import Control.Applicative
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

-- $setup
-- >>> import Data.Maybe
-- >>> import qualified Data.ByteString.Lazy.Char8 as L
-- >>> import Data.Text ()
-- >>> let v = decode (L.pack "{\"foo\": {\"baz\": 3.14}, \"bar\": [123, false, null]}") :: Maybe Value

data ValueIx = ArrIx Int | ObjIx T.Text

valueAt :: ValueIx -> SimpleIndexedLens ValueIx (Maybe Value) (Maybe Value)
valueAt k = index $ \f v -> (go k v) <$> f k (lu k v) where
  go (ObjIx ix) (Just (Object o)) Nothing = Just $ Object $ HMS.delete ix o
  go (ObjIx ix) (Just (Object o)) (Just v) = Just $ Object $ HMS.insert ix v o
  go (ArrIx ix) (Just (Array a)) Nothing = Just $ Array $ a V.// [(ix, Null)]
  go (ArrIx ix) (Just (Array a)) (Just v) = Just $ Array $ a V.// [(ix, v)]
  go _ v _ = v

  lu (ObjIx ix) (Just (Object o)) = HMS.lookup ix o
  lu (ArrIx ix) (Just (Array a)) | ix >= 0 && ix < V.length a = Just $ a V.! ix
  lu _ _ = Nothing

-- | Lens of Array
--
arr :: Int -> SimpleIndexedLens ValueIx (Maybe Value) (Maybe Value)
arr = valueAt . ArrIx

-- | Lens of Object
--
-- >>> v ^. obj (T.pack "foo") . obj (T.pack "baz")
-- Just (Number 3.14)
obj :: T.Text -> SimpleIndexedLens ValueIx (Maybe Value) (Maybe Value)
obj = valueAt . ObjIx
