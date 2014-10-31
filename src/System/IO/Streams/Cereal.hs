{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sytem.IO.Streams.Cereal
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Michael Xavier
-- Stability   :  experimental
--
-- io-streams interface to the cereal binary serialization library.
----------------------------------------------------------------------------
module System.IO.Streams.Cereal
    ( getFromStream
    , putToStream
    , getEachStream
    , putEachStream
    , GetException(..)
    ) where

-------------------------------------------------------------------------------
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
    show (GetException s) = "Get exception: " ++ s

instance Exception GetException



-------------------------------------------------------------------------------
-- | Convert a 'Put' into an 'InputStream'
--
-- Example:
--
-- >>> putToStream (put False)
putToStream :: Put -> IO (InputStream ByteString)
putToStream = Streams.fromLazyByteString . runPutLazy
{-# INLINE putToStream #-}


-------------------------------------------------------------------------------
-- | Convert a stream of individual serialized 'ByteString's to a stream
-- of Results. Throws a GetException on error.
--
-- Example:
--
-- >>> Streams.toList =<< getEachStream (get :: Get String) =<< Streams.fromList (map (runPut . put) ["foo", "bar"])
-- ["foo","bar"]
getEachStream :: Get r -> InputStream ByteString -> IO (InputStream r)
getEachStream g = Streams.mapM (either (throwIO . GetException) return . runGet g)
{-# INLINE getEachStream #-}


-------------------------------------------------------------------------------
-- | Convert a stream of serializable objects into a stream of
-- individual 'ByteString's
-- Example:
--
-- >>> Streams.toList =<< getEachStream (get :: Get String) =<< putEachStream put =<< Streams.fromList ["foo","bar"]
-- ["foo","bar"]
putEachStream :: Putter r -> InputStream r -> IO (InputStream ByteString)
putEachStream p = Streams.map (runPut . p)
{-# INLINE putEachStream #-}


-------------------------------------------------------------------------------
-- | Take a 'Get' and an 'InputStream' and deserialize a
-- value. Consumes only as much input as necessary to deserialize the
-- value. Unconsumed input is left on the 'InputStream'. If there is
-- an error while deserializing, a 'GetException' is thrown.
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
getFromStream :: Get r -> InputStream ByteString -> IO r
getFromStream = getFromStreamInternal runGetPartial feed
{-# INLINE getFromStream #-}


-------------------------------------------------------------------------------
feed :: Result r -> ByteString -> Result r
feed (Partial f) bs = f bs
feed (Done r x) bs = Done r $ x <> bs
feed (Fail s x) bs = Fail s $ x <> bs

-------------------------------------------------------------------------------
getFromStreamInternal
    :: (Get r -> ByteString -> Result r)
    -> (Result r -> ByteString -> Result r)
    -> Get r
    -> InputStream ByteString
    -> IO r
getFromStreamInternal getFunc feedFunc g is =
    Streams.read is >>=
    maybe (finish $ getFunc g "")
          (\s -> if S.null s
                   then getFromStreamInternal getFunc feedFunc g is
                   else go $! getFunc g s)
  where
    leftover x = unless (S.null x) $ Streams.unRead x is

    finish k = let k' = feedFunc (feedFunc k "") ""
               in case k' of
                    Fail _ x -> leftover x >> err k'
                    Partial _  -> err k' -- should be impossible
                    Done r x   -> leftover x >> return r

    err r = let (Left s) = eitherResult r in throwIO $ GetException s
    eitherResult (Done _ r)     = Right r
    eitherResult (Fail msg _)   = Left msg
    eitherResult _              = Left "Result: incomplete input"

    go r@(Fail _ x) = leftover x >> err r
    go (Done r x)     = leftover x >> return r
    go r              = Streams.read is >>=
                        maybe (finish r)
                              (\s -> if S.null s
                                       then go r
                                       else go $! feedFunc r s)
