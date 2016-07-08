{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-------------------------------------------------------------------------------

import           Criterion.Main
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (toStrict, toChunks)
import           Data.Conduit
import qualified Data.Conduit.Cereal as Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.Conduit.Binary as Conduit
import           Data.Monoid
import           Data.Serialize
import           GHC.Generics
import           Control.Monad.IO.Class
import           Control.Monad   (replicateM_, replicateM)
import           Control.Exception (evaluate)

-------------------------------------------------------------------------------

import qualified System.IO.Streams        as Streams
import           System.IO.Streams.Cereal

-------------------------------------------------------------------------------

main :: IO ()
main = do
  let lstring = mconcat $ map (runPutLazy . put) foos
      foos = map exFoo [0..1000]
      exFoo x = Foo x "oh look, a Foo!"
  defaultMain
    [ bgroup "decode one element from cereal-streams" [
         bench "1000 items" $ whnfIO $ benchCS lstring ]
    , bgroup "decode one element cereal-conduit" [
         bench "1000 items" $ whnfIO $ benchCC lstring ]
    , bgroup "decode 1000 elements from cereal-streams" [
         bench "1000 items" $ whnfIO $ benchCSA lstring ]
    , bgroup "decode 1000 elements cereal-conduit" [
         bench "1000 items" $ whnfIO $ benchCCA lstring ]
    ]

benchCS lstring = do
    s <- decodeInputStream =<< Streams.fromLazyByteString lstring
    a <- Streams.read s :: IO (Maybe Foo)
    evaluate a

benchCC lstring = do
    Conduit.sourceLbs lstring =$= Conduit.conduitGet2 (get :: Get Foo) $$ do
        a <- await
        liftIO (evaluate a)

benchCSA lstring = do
    s <- decodeInputStream =<< Streams.fromLazyByteString lstring
    replicateM_ 1000 $ do
        a <- Streams.read s :: IO (Maybe Foo)
        evaluate a

benchCCA lstring = do
    Conduit.sourceLbs lstring =$= Conduit.conduitGet2 (get :: Get Foo) $$
        replicateM_ 1000 $ do
            a <- await
            liftIO (evaluate a)

-------------------------------------------------------------------------------

data Foo = Foo Int ByteString deriving (Generic, Show, Eq)

instance Serialize Foo
