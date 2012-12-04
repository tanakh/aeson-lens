{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.Aeson.Lens (
  -- * Lenses
  nth, nth',
  key, key',

  asDouble,
  asText,
  asBool,

  -- * Traversals
  traverseArray, traverseArray',
  traverseObject, traverseObject',

  -- * Generic Indexing
  ValueIx(..),
  valueAt,
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict as HMS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Vector         as V

-- $setup
-- >>> import Data.Maybe
-- >>> import qualified Data.ByteString.Lazy.Char8 as L
-- >>> import Data.Text ()

data ValueIx = ArrIx Int | ObjIx T.Text

-- | Lens of Value
valueAt :: (FromJSON u, ToJSON v)
           => ValueIx
           -> IndexedLens ValueIx (Maybe Value) (Maybe Value) (Maybe u) (Maybe v)
valueAt k = index $ \f (fmap toJSON -> v) -> go k v <$> f k (lu k v) where
  go (ObjIx ix) (Just (Object o)) Nothing  = Just $ Object $ HMS.delete ix o
  go (ObjIx ix) (Just (Object o)) (Just v) = Just $ Object $ HMS.insert ix (toJSON v) o
  go (ObjIx ix) _                 (Just v) = Just $ Object $ HMS.fromList [(ix, toJSON v)]
  go (ArrIx ix) (Just (Array  a)) Nothing  = Just $ Array $ updateV ix Null a
  go (ArrIx ix) (Just (Array  a)) (Just v) = Just $ Array $ updateV ix (toJSON v) a
  go (ArrIx ix) _                 (Just v) = Just $ Array $ updateV ix (toJSON v) mempty
  go _ v _ = v

  lu (ObjIx ix) (Just (Object o)) = fromJSONMaybe =<< HMS.lookup ix o
  lu (ArrIx ix) (Just (Array a)) | ix >= 0 && ix < V.length a = fromJSONMaybe $ a V.! ix
  lu _ _ = Nothing
{-# INLINE valueAt #-}

updateV :: Int -> Value -> V.Vector Value -> V.Vector Value
updateV i v a
  | i >= V.length a =
    updateV i v $ V.generate (i + 1) $ \ii -> fromMaybe Null $ a V.!? ii
  | otherwise =
    a V.// [(i, v)]
{-# INLINE updateV #-}

fromJSONMaybe :: FromJSON a => Value -> Maybe a
fromJSONMaybe v = case fromJSON v of
  Error   _ -> Nothing
  Success a -> Just a
{-# INLINE fromJSONMaybe #-}

-- | Lens of Array
--
-- >>> let v = decode (L.pack "{\"foo\": {\"baz\": 3.14}, \"bar\": [123, false, null]}") :: Maybe Value
-- >>> v ^. key (T.pack "bar") . nth 1 :: Maybe Bool
-- Just False
-- >>> v ^. key (T.pack "bar") . nth 1 :: Maybe String
-- Nothing
-- >>> v ^. key (T.pack "bar") . nth 3 :: Maybe Value
-- Nothing
-- >>> v ^. nth 0 :: Maybe Value
-- Nothing
-- >>> let x = nth 0 .~ Just 1 $ Nothing
-- >>> L.unpack $ encode x
-- "[1]"
-- >>> let y = nth 1 .~ Just "hoge" $ x
-- >>> L.unpack $ encode y
-- "[1,\"hoge\"]"
-- >>> let z = nth 0 .~ Just False $ y
-- >>> L.unpack $ encode z
-- "[false,\"hoge\"]"
--
-- >>> let v = decode (L.pack "[]") :: Maybe Value
-- >>> v & nth 0 .~ Just "hello"
-- Just (Array (fromList [String "hello"]))
-- >>> v & nth 1 .~ Just "hello"
-- Just (Array (fromList [Null,String "hello"]))

nth :: (FromJSON v, ToJSON v)
       => Int
       -> SimpleIndexedLens ValueIx (Maybe Value) (Maybe v)
nth = nth'
{-# INLINE nth #-}

nth' :: (FromJSON u, ToJSON v)
       => Int
       -> IndexedLens ValueIx (Maybe Value) (Maybe Value) (Maybe u) (Maybe v)
nth' = valueAt . ArrIx
{-# INLINE nth' #-}

-- | Lens of Object
--
-- >>> let v = decode (L.pack "{\"foo\": {\"baz\": 3.14}, \"bar\": [123, false, null]}") :: Maybe Value
-- >>> v ^. key (T.pack "foo") . key (T.pack "baz") :: Maybe Double
-- Just 3.14
-- >>> v ^. key (T.pack "foo") . key (T.pack "baz") :: Maybe Object
-- Nothing
-- >>> v ^. key (T.pack "foo") . key (T.pack "hoge") :: Maybe Value
-- Nothing
-- >>> v ^. key (T.pack "hoge") :: Maybe Value
-- Nothing
-- >>> let w = key (T.pack "a") .~ Just 2.23 $ Nothing
-- >>> L.unpack $ encode w
-- "{\"a\":2.23}"
-- >>> let x = key (T.pack "b") . key (T.pack "c") .~ Just True $ w
-- >>> L.unpack $ encode x
-- "{\"b\":{\"c\":true},\"a\":2.23}"
key :: (FromJSON v, ToJSON v)
       => T.Text
       -> SimpleIndexedLens ValueIx (Maybe Value) (Maybe v)
key = key'
{-# INLINE key #-}

key' :: (FromJSON u, ToJSON v)
       => T.Text
       -> IndexedLens ValueIx (Maybe Value) (Maybe Value) (Maybe u) (Maybe v)
key' = valueAt . ObjIx
{-# INLINE key' #-}

-- | Indexed traversal of Array
--
-- >>> let v = decode (L.pack "[1, true, null]") :: Maybe Value
-- >>> catMaybes . toListOf traverseArray $ v :: [Value]
-- [Number 1,Bool True,Null]
-- >>> let w = decode (L.pack "[{\"name\": \"tanakh\", \"age\": 29}, {\"name\": \"nushio\", \"age\": 28}]") :: Maybe Value
-- >>> catMaybes . toListOf (traverseArray . key (T.pack "name")) $ w :: [T.Text]
-- ["tanakh","nushio"]
traverseArray :: (FromJSON v, ToJSON v)
                 => SimpleIndexedTraversal Int (Maybe Value) (Maybe v)
traverseArray = traverseArray'
{-# INLINE traverseArray #-}

-- | Type-changing indexed traversal of an Array
traverseArray' :: (FromJSON u, ToJSON v)
               => IndexedTraversal Int (Maybe Value) (Maybe Value) (Maybe u) (Maybe v)
traverseArray' = index $ \f m -> case m of
  Just (Array (map fromJSONMaybe . V.toList -> v)) ->
    Just . Array . V.fromList . map toJSON . catMaybes <$> itraverse f v
  v -> pure v
{-# INLINE traverseArray' #-}

-- | Indexed traversal of Object
--
-- >>> let w = decode (L.pack "[{\"name\": \"tanakh\", \"age\": 29}, {\"name\": \"nushio\", \"age\": 28}]") :: Maybe Value
-- >>> catMaybes . toListOf (traverseArray . traverseObject) $ w :: [Value]
-- [String "tanakh",Number 29,String "nushio",Number 28]
traverseObject :: (FromJSON v, ToJSON v)
                  => SimpleIndexedTraversal T.Text (Maybe Value) (Maybe v)
traverseObject = traverseObject'
{-# INLINE traverseObject #-}

-- | Type-changing indexed traversal of Object
traverseObject' :: (FromJSON u, ToJSON v)
                  => IndexedTraversal T.Text (Maybe Value) (Maybe Value) (Maybe u) (Maybe v)
traverseObject' = index $ \f m -> case m of
  Just (Object (expand . HMS.toList -> v)) ->
    Just . Object . HMS.fromList . catMaybes . collapse <$> withIndex traverseAssocList f v
  v -> pure v
  where
  expand = map (_2 %~ fromJSONMaybe)
  collapse = map (\(a, b) -> (a, ) . toJSON <$> b)
{-# INLINE traverseObject' #-}

traverseAssocList :: IndexedTraversal k [(k, u)] [(k, v)] u v
traverseAssocList = index $ \f m -> go f m where
  go _ [] = pure []
  go f ((k, v): xs) = (\v' ys -> (k, v') : ys) <$> f k v <*> go f xs
{-# INLINE traverseAssocList #-}

-- | Lens of Double
--
-- >>> let v = decode (L.pack "{\"foo\": {\"baz\": 3.14}, \"bar\": [123, false, null]}") :: Maybe Value
-- >>> v ^. key (T.pack "foo") . key (T.pack "baz") . asDouble
-- Just 3.14
-- >>> v ^. key (T.pack "bar") . asDouble
-- Nothing
-- >>> v ^. key (T.pack "hoge") . asDouble
-- Nothing
asDouble :: SimpleLens (Maybe Value) (Maybe Double)
asDouble = as
{-# INLINE asDouble #-}

-- | Lens of Text
--
-- >>> let v = decode (L.pack "{\"foo\": {\"baz\": \"3.14\"}, \"bar\": [123, false, null]}") :: Maybe Value
-- >>> v ^. key (T.pack "foo") . key (T.pack "baz") . asText
-- Just "3.14"
-- >>> v ^. key (T.pack "bar") . asText
-- Nothing
-- >>> v ^. key (T.pack "hoge") . asText
-- Nothing
asText :: SimpleLens (Maybe Value) (Maybe T.Text)
asText = as
{-# INLINE asText #-}

-- | Lens of Bool
--
-- >>> let v = decode (L.pack "{\"foo\": {\"baz\": false}, \"bar\": [123, false, null]}") :: Maybe Value
-- >>> v ^. key (T.pack "foo") . key (T.pack "baz") . asBool
-- Just False
-- >>> v ^. key (T.pack "bar") . asBool
-- Nothing
-- >>> v ^. key (T.pack "hoge") . asBool
-- Nothing
asBool :: SimpleLens (Maybe Value) (Maybe Bool)
asBool = as
{-# INLINE asBool #-}

as :: (ToJSON v, FromJSON v)
      => SimpleLens (Maybe Value) (Maybe v)
as f x = toJSON <$$> f (fromJSONMaybe =<< x)
  where
  (<$$>) = fmap . fmap
{-# INLINE as #-}
