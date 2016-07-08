{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sytem.IO.Streams.Cereal
-- Copyright   :  Soostone Inc, Winterland
-- License     :  BSD3
--
-- Maintainer  :  Michael Xavier, Winterland
-- Stability   :  experimental
--
-- io-streams interface to the cereal binary serialization library.
----------------------------------------------------------------------------

module System.IO.Streams.Cereal
    (
    -- * single element serialize/deSerialize
      getFromStream
    , putToStream
    -- * 'InputStream' serialize/deSerialize
    , getInputStream
    , deSerializeInputStream
    , putInputStream
    , serializeInputStream
    -- * 'OutputStream' serialize
    , putOutputStream
    , serializeOutputStream
    -- * exception type
    , GetException(..)
    ) where

-------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Exception      (Exception, throwIO)
import           Control.Monad
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as S
import           Data.Monoid
import           Data.Serialize
import           Data.Typeable
import qualified System.IO.Streams      as Streams
import           System.IO.Streams.Core

-------------------------------------------------------------------------------

data GetException = GetException String
  deriving (Typeable)

instance Show GetException where
    show (GetException s) = "Get parse error: " ++ s

instance Exception GetException

-------------------------------------------------------------------------------

-- | write a 'Put' to an 'OutputStream'
--
putToStream :: Put -> OutputStream ByteString -> IO ()
putToStream p = Streams.write (Just (runPut p))
{-# INLINE putToStream #-}

-------------------------------------------------------------------------------

-- | Take a 'Get' and an 'InputStream' and deserialize a
-- value. Consumes only as much input as necessary to deserialize the
-- value. Unconsumed input will be unread. If there is
-- an error while deserializing, a 'GetException' is thrown, and
-- unconsumed part will be unread.
--
-- Examples:
--
-- >>> getFromStream (get :: Get String) =<< putToStream (put "serialize me")
-- "serialize me"
-- >>> getFromStream (get :: Get String) =<< Streams.fromByteString (Data.ByteString.drop 1 $ runPut $ put ("serialize me" :: String))
-- *** Exception: Get exception: too few bytes
-- From:	demandInput
-- <BLANKLINE>
-- <BLANKLINE>
getFromStream :: Get r -> InputStream ByteString -> IO (Maybe r)
getFromStream get is =
    Streams.read is >>= maybe (return Nothing) (go . runGetPartial get)
  where
    go (Fail msg s) = do
        Streams.unRead s is
        throwIO (GetException msg)
    go (Done r s) = do
         unless (S.null s) (Streams.unRead s is)
         return (Just r)
    go c@(Partial cont) =
        Streams.read is >>= maybe (go (cont S.empty))   -- use 'empty' to notify cereal ending.
        (\ s -> if S.null s then go c else go (cont s))
{-# INLINE getFromStream #-}

-------------------------------------------------------------------------------

-- | Convert a stream of individual serialized 'ByteString's to a stream
-- of Results. Throws a GetException on error.
--
-- Example:
--
-- >>> Streams.toList =<< getInputStream (get :: Get String) =<< Streams.fromList (map (runPut . put) ["foo", "bar"])
-- ["foo","bar"]
getInputStream :: Get r -> InputStream ByteString -> IO (InputStream r)
getInputStream g is = makeInputStream (getFromStream g is)
{-# INLINE getInputStream #-}

-- | typeclass version of 'getInputStream'
deSerializeInputStream :: Serialize r => InputStream ByteString -> IO (InputStream r)
deSerializeInputStream = getInputStream get

-------------------------------------------------------------------------------

-- | Convert a stream of serializable objects into a stream of
-- individual 'ByteString's with a 'Putter', this function are used in
-- round-trip test.
-- Example:
--
-- >>> Streams.toList =<< getInputStream (get :: Get String) =<< serializeInputStream =<< Streams.fromList ["foo","bar"]
-- ["foo","bar"]
putInputStream :: Putter r -> InputStream r -> IO (InputStream ByteString)
putInputStream p = Streams.map (runPut . p)
{-# INLINE putInputStream #-}

-- | typeclass version of 'putInputStream'
serializeInputStream :: Serialize r => InputStream r -> IO (InputStream ByteString)
serializeInputStream = putInputStream put

-------------------------------------------------------------------------------

-- | create an 'OutputStream' of serializable values from an 'OutputStream'
-- of bytestrings with a 'Putter'.
putOutputStream :: Putter r -> OutputStream ByteString -> IO (OutputStream r)
putOutputStream p = Streams.contramap (runPut . p)
{-# INLINE putOutputStream #-}

-- | typeclass version of 'putOutputStream'
serializeOutputStream :: Serialize r => OutputStream ByteString -> IO (OutputStream r)
serializeOutputStream = Streams.contramap (runPut . put)
