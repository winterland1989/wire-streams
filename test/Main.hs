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
import           System.IO.Streams
import qualified System.IO.Streams as S
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit         hiding (assert)
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           System.IO.Streams.Cereal
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "cereal-io-streams"
  [
    testProperty "serialization roundtrips Foo" prop_roundtrip_Foo
  , testCase "partial input" test_partial
  , testCase "excess preceding input" test_excess_head
  , testCase "excess remaining input left in stream" test_excess_tail
  ]


-------------------------------------------------------------------------------
prop_roundtrip_Foo = monadicIO $ do
    a   <- (pick arbitrary :: PropertyM IO Foo)
    res <- run $
      getFromStream get =<< putToStream (put a)
    assert $ a == res


-------------------------------------------------------------------------------
test_partial = do
  let s = mutatePut $ BS.drop 1
  assertGetException $ getFromStream (get :: Get Foo) =<< fromByteString s

-------------------------------------------------------------------------------
test_excess_tail = do
  let s = mutatePut $ (<> "extra")
  inS <- fromByteString s
  getFromStream (get :: Get Foo) inS
  remainder <- smappend inS
  remainder @?= "extra"

-------------------------------------------------------------------------------
smappend = S.fold mappend mempty

-------------------------------------------------------------------------------
test_excess_head = do
  let s = mutatePut $ ("extra" <>)
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
