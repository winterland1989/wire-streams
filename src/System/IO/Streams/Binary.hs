{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sytem.IO.Streams.Binary
-- Copyright   :  Petter Bergman, Winterland
-- License     :  BSD3
--
-- Maintainer  :  Winterland
-- Stability   :  experimental
--
-- Use cereal to encode/decode io-streams.
--------------------------------------------------------------------------------

module System.IO.Streams.Binary (
    -- * single element encode/decode
      getFromStream
    , decodeFromStream
    , putToStream
    -- * 'InputStream' encode/decode
    , getInputStream
    , decodeInputStream
    -- * 'OutputStream' encode
    , putOutputStream
    , encodeOutputStream
    -- * exception type
    , DecodeException(..)
    ) where

--------------------------------------------------------------------------------

import           Control.Exception            (Exception, throw)
import           Control.Monad                (unless)
import           Data.Binary                  (Binary, get, put)
import           Data.Binary.Get              (ByteOffset, Decoder (..), Get,
                                               pushChunk, pushEndOfInput,
                                               runGetIncremental)
import           Data.Binary.Put              (runPut, Put)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import           Data.Typeable                (Typeable)
import           System.IO.Streams            (InputStream, OutputStream)
import qualified System.IO.Streams            as Streams
import           System.IO.Streams.ByteString (writeLazyByteString)

--------------------------------------------------------------------------------

-- | An Exception raised when decoding fails.
data DecodeException = DecodeException ByteOffset String
  deriving (Typeable)

instance Show DecodeException where
  show (DecodeException offset message) =
        "System.IO.Streams.Binary: binary decode exception: offset " ++ show offset ++ ", " ++ show message

instance Exception DecodeException

--------------------------------------------------------------------------------

-- | Write an instance of 'Binary' to an 'InputStream'.
putToStream :: Binary a => Maybe a -> OutputStream ByteString -> IO ()
putToStream Nothing  os = Streams.write Nothing os
putToStream (Just x) os = writeLazyByteString ((runPut . put) x) os
{-# INLINE putToStream #-}

--------------------------------------------------------------------------------

getFromStream :: Get a -> InputStream ByteString -> IO (Maybe a)
getFromStream g is = do
    let decoder = runGetIncremental g
    Streams.read is >>= maybe (return Nothing)
        (\s -> if S.null s then go decoder else go $ pushChunk decoder s)
  where go (Fail _ offset message) = throw $ DecodeException offset message
        go (Done s _ x) = do
            unless (S.null s) (Streams.unRead s is)
            return (Just x)
        go  decoder' =
            Streams.read is >>=
               maybe (go $ pushEndOfInput decoder')
                     (\s -> if S.null s then go decoder' else go $ pushChunk decoder' s)
{-# INLINE getFromStream #-}


decodeFromStream :: Binary a => InputStream ByteString -> IO (Maybe a)
decodeFromStream = getFromStream get
{-# INLINE decodeFromStream #-}

--------------------------------------------------------------------------------

-- | Transform an 'InputStream' over byte strings to an 'InputStream' yielding
--   values of type a, throwing a 'DecodeException' if the decoding fails.
getInputStream :: Get a -> InputStream ByteString -> IO (InputStream a)
getInputStream g = Streams.makeInputStream . getFromStream g
{-# INLINE getInputStream #-}

-- | Transform an 'InputStream' over byte strings to an 'InputStream' yielding
--   values of type a, throwing a 'DecodeException' if the decoding fails.
decodeInputStream :: Binary a => InputStream ByteString -> IO (InputStream a)
decodeInputStream = Streams.makeInputStream . decodeFromStream

--------------------------------------------------------------------------------

-- | Transform an 'OutputStream' accepting byte strings to an 'OutputStream'
--   accepting values of type a.
putOutputStream :: (a -> Put) -> OutputStream ByteString -> IO (OutputStream a)
putOutputStream p os = Streams.makeOutputStream $ \ ma ->
    case ma of Nothing -> Streams.write Nothing os
               Just a -> writeLazyByteString (runPut (p a)) os
{-# INLINE putOutputStream #-}

-- | Transform an 'OutputStream' accepting byte strings to an 'OutputStream'
--   accepting values of type a.
encodeOutputStream :: Binary a => OutputStream ByteString -> IO (OutputStream a)
encodeOutputStream os = Streams.makeOutputStream (\ ma -> putToStream ma os)

