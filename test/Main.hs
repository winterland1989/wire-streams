{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Control.Exception        hiding (assert)
import qualified Data.ByteString          as BS
import           Data.DeriveTH
import           Data.Monoid
import           Data.Serialize
import           GHC.Generics
import           System.IO.Streams        hiding (map)
import qualified System.IO.Streams        as Streams
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit         hiding (assert)
import           Test.Tasty.QuickCheck

import           Control.Monad
import qualified Control.Monad.State      as MS
import qualified Data.ByteString.Char8    as BS8
import qualified Data.ByteString.Lazy     as LBS
-------------------------------------------------------------------------------
import           System.IO.Streams.Cereal
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "cereal-io-streams"
    [ testProperty "serialization roundtrips Foo" roundTripFoo
    , testProperty "serialization roundtrips Foo unpredictable chunking" roundTripFoo'
    , testCase "partial input" partialInput
    , testCase "excess preceding input" excessPrefix
    , testCase "excess remaining input left in stream" excessSuffix
    ]

-------------------------------------------------------------------------------

roundTripFoo = monadicIO $ do
    a   <- (pick arbitrary :: PropertyM IO Foo)
    Just res <- run $ do
        is <- nullInput
        unRead a is
        getFromStream get =<< serializeInputStream is
    assert $ a == res

-------------------------------------------------------------------------------

roundTripFoo' = monadicIO $ do
    as   <- (pick arbitrary :: PropertyM IO [Foo])
    Positive csize <- pick arbitrary
    res <- run $ do
      lbs <- fmap (LBS.fromChunks . rechunk csize . mconcat) . Streams.toList =<< serializeInputStream =<< Streams.fromList as
      is <- fromLazyByteString lbs
      Streams.toList =<< deSerializeInputStream is
    assert $ as == res

rechunk :: Int -> BS.ByteString -> [BS.ByteString]
rechunk n bs = fst $ MS.execState go ([], bs)
  where
    go :: MS.State ([BS.ByteString], BS.ByteString) ()
    go = do
        (chunks, rmning) <- MS.get
        unless (BS.null rmning) $ do
            let (chunk, rmning') = BS.splitAt n rmning
            MS.put (chunks ++ [chunk], rmning')
            go

-------------------------------------------------------------------------------

partialInput = do
  let s = mutatePut $ BS.drop 1
  assertGetException $ getFromStream (get :: Get Foo) =<< fromByteString s

-------------------------------------------------------------------------------

excessSuffix = do
  let s = mutatePut (<> "extra")
  inS <- fromByteString s
  getFromStream (get :: Get Foo) inS
  remainder <- smappend inS
  remainder @?= "extra"

-------------------------------------------------------------------------------

smappend = Streams.fold mappend mempty

-------------------------------------------------------------------------------

excessPrefix = do
  let s = mutatePut ("extra" <>)
  assertGetException $ getFromStream (get :: Get Foo) =<< fromByteString s


-------------------------------------------------------------------------------

mutatePut f = f $ runPut $ put $ Foo 42 "yup"

-------------------------------------------------------------------------------

assertGetException a = do
  res <- try a
  case res of
    Left (GetException _) -> return ()
    (Right r)             -> assertFailure $ "Expected a GetException but got " ++ show r

-------------------------------------------------------------------------------

data Foo = Foo Int String deriving (Generic,Show,Eq)

instance Serialize Foo
$(derive makeArbitrary ''Foo)
