{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Criterion.Main
import           Data.ByteString          (ByteString)
import           Data.Monoid
import           Data.Serialize
import           GHC.Generics
-------------------------------------------------------------------------------
import qualified System.IO.Streams        as Streams
import           System.IO.Streams.Cereal
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [
   bgroup "getEachStream" [
     bench "1000 items" $ whnfIO $ do
       -- can't get "env" to work because it demands an NFData on
       -- inputstream/putputstream
       (is, os) <- setup
       Streams.connectTo os =<< getEachStream get is
   ]
 ]


-------------------------------------------------------------------------------
setup :: IO (Streams.InputStream ByteString, Streams.OutputStream Foo)
setup = do
  is <- Streams.fromLazyByteString lstring
  os <- Streams.nullOutput
  return (is, os)
  where
    lstring = mconcat $ map (runPutLazy . put) foos
    foos = replicate 1000 exFoo
    exFoo = Foo 42 "oh look, a Foo!"


-------------------------------------------------------------------------------
data Foo = Foo Int ByteString deriving (Generic,Show,Eq)

instance Serialize Foo
