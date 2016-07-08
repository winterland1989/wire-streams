{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where

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
    [ bgroup "getEachStream cereal-io-streams" [
         bench "1000 items" $ whnfIO $ benchCIS lstring ]
    , bgroup "getEachStream cereal-conduit" [
         bench "1000 items" $ whnfIO $ benchCC lstring ]
    ]

benchCIS lstring = do
  s <- deSerializeInputStream =<< Streams.fromLazyByteString lstring
  (a :: Maybe Foo) <- Streams.read s
  evaluate a

benchCC lstring = do
    Conduit.sourceLbs lstring =$= Conduit.conduitGet2 get' $$ do
        a <- await
        liftIO (evaluate a)

get' :: Get Foo
get' = get

-------------------------------------------------------------------------------

data Foo = Foo Int ByteString deriving (Generic,Show,Eq)

instance Serialize Foo
