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
-- Use cereal to encode/decode io-streams.
----------------------------------------------------------------------------

module System.IO.Streams.Cereal
    (
    -- * single element encode/decode
      getFromStream
    , putToStream
    -- * 'InputStream' encode/decode
    , getInputStream
    , decodeInputStream
    , putInputStream
    , encodeInputStream
    -- * 'OutputStream' encode
    , putOutputStream
    , encodeOutputStream
    -- * exception type
    , DecodeException(..)
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

data DecodeException = DecodeException String
  deriving (Typeable)

instance Show DecodeException where
    show (DecodeException s) = "System.IO.Streams.Cereal: cereal decode exception: " ++ s

instance Exception DecodeException

-------------------------------------------------------------------------------

-- | write a 'Put' to an 'OutputStream'
--
putToStream :: Put -> OutputStream ByteString -> IO ()
putToStream p = Streams.write (Just (runPut p))
{-# INLINE putToStream #-}

-------------------------------------------------------------------------------

-- | Take a 'Get' and an 'InputStream' and decode a
-- value. Consumes only as much input as necessary to decode the
-- value. Unconsumed input will be unread. If there is
-- an error while deserializing, a 'DecodeException' is thrown, and
-- unconsumed part will be unread. To simplify upstream generation,
-- all empty 'ByteString' will be filtered out and not passed to cereal,
-- only EOFs/Nothing will close a cereal decoder.
--
-- Examples:
--
-- >>> import qualified System.IO.Streams as Streams
-- >>> getFromStream (get :: Get String) =<< Streams.fromByteString (Data.ByteString.drop 1 $ runPut $ put "encode me")
-- *** Exception: System.IO.Streams.Cereal: cereal decode exception: too few bytes
-- From:	demandInput
-- <BLANKLINE>
getFromStream :: Get r -> InputStream ByteString -> IO (Maybe r)
getFromStream g is =
    Streams.read is >>= maybe (return Nothing) (go . runGetPartial g)
  where
    go (Fail msg s) = do
        Streams.unRead s is
        throwIO (DecodeException msg)
    go (Done r s) = do
         unless (S.null s) (Streams.unRead s is)
         return (Just r)
    go c@(Partial cont) =
        Streams.read is >>= maybe (go (cont S.empty))   -- use 'empty' to notify cereal ending.
        (\ s -> if S.null s then go c else go (cont s))
{-# INLINE getFromStream #-}

-------------------------------------------------------------------------------

-- | Convert a stream of individual encoded 'ByteString's to a stream
-- of Results. Throws a 'DecodeException' on error.
--
-- Example:
--
-- >>> Streams.toList =<< getInputStream (get :: Get String) =<< Streams.fromList (map (runPut . put) ["foo", "bar"])
-- ["foo","bar"]
getInputStream :: Get r -> InputStream ByteString -> IO (InputStream r)
getInputStream g is = makeInputStream (getFromStream g is)
{-# INLINE getInputStream #-}

-- | typeclass version of 'getInputStream'
decodeInputStream :: Serialize r => InputStream ByteString -> IO (InputStream r)
decodeInputStream = getInputStream get

-------------------------------------------------------------------------------

-- | Convert a stream of serializable objects into a stream of
-- individual 'ByteString's with a 'Putter', while most of the time
-- these function are not needed, they can be used in round-trip test.
-- Example:
--
-- >>> Streams.toList =<< getInputStream (get :: Get String) =<< encodeInputStream =<< Streams.fromList ["foo","bar"]
-- ["foo","bar"]
putInputStream :: Putter r -> InputStream r -> IO (InputStream ByteString)
putInputStream p = Streams.map (runPut . p)
{-# INLINE putInputStream #-}

-- | typeclass version of 'putInputStream'
encodeInputStream :: Serialize r => InputStream r -> IO (InputStream ByteString)
encodeInputStream = putInputStream put

-------------------------------------------------------------------------------

-- | create an 'OutputStream' of serializable values from an 'OutputStream'
-- of bytestrings with a 'Putter'.
putOutputStream :: Putter r -> OutputStream ByteString -> IO (OutputStream r)
putOutputStream p = Streams.contramap (runPut . p)
{-# INLINE putOutputStream #-}

-- | typeclass version of 'putOutputStream'
encodeOutputStream :: Serialize r => OutputStream ByteString -> IO (OutputStream r)
encodeOutputStream = Streams.contramap (runPut . put)
