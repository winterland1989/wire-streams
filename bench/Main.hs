{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Criterion.Main
import           Data.ByteString          (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Cereal as Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.Conduit.Binary as Conduit
import           Data.Monoid
import           Data.Serialize
import           GHC.Generics
-------------------------------------------------------------------------------
import qualified System.IO.Streams        as Streams
import           System.IO.Streams.Cereal
-------------------------------------------------------------------------------

main :: IO ()
main = do
  let lstring = mconcat $ map (runPutLazy . put) foos
      foos = replicate 1000 exFoo
      exFoo = Foo 42 "oh look, a Foo!"
  defaultMain
    [ bgroup "getEachStream cereal-io-streams" [
         bench "1000 items" $ whnfIO $ benchCIS lstring ]
    , bgroup "getEachStream cereal-conduit" [
         bench "1000 items" $ whnfIO $ benchCC lstring ]
    ]
 where


benchCIS lstring = do
  os <- Streams.nullOutput
  Streams.connectTo os =<< getEachStream get' =<< Streams.fromLazyByteString lstring

benchCC lstring = do
  Conduit.sourceLbs lstring $= Conduit.conduitGet get' $$ Conduit.sinkNull


get' :: Get Foo
get' = get


-------------------------------------------------------------------------------
data Foo = Foo Int ByteString deriving (Generic,Show,Eq)

instance Serialize Foo
