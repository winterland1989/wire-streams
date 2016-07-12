{-# LANGUAGE ScopedTypeVariables #-}
module EncodeDecodeCereal ( tests ) where

import           Control.Exception                         (catch,evaluate)
import           Data.Serialize                            (Serialize)
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString                           as S
import           Distribution.TestSuite                    (Test)
import           Distribution.TestSuite.QuickCheck         (testProperty)
import           System.IO.Streams                         (write)
import           System.IO.Streams.Cereal                  (decodeInputStream,
                                                            encodeOutputStream,
                                                            DecodeException)
import           System.IO.Streams.List                    (outputToList,
                                                            fromList,
                                                            toList,
                                                            writeList)
import           Test.QuickCheck.Property                  (Property)
import           Test.QuickCheck.Monadic                   (monadicIO,
                                                            assert,
                                                            run)


-- Using binary-streams, decode from a list of bytestrings
decode :: Serialize a => [ByteString] -> IO [a]
decode ss = fromList ss >>= decodeInputStream >>= toList

-- Using binary-streams, encode to a list of bytestrings
encode :: Serialize a => [a] -> IO [ByteString]
encode xs = outputToList $ \os ->
  do
    bos <- encodeOutputStream os
    writeList xs bos
    write Nothing bos

-- Encode something, then decode it and make sure we get the same thing back.
encodeDecodeEq :: (Serialize a,Eq a) => [a] -> Property
encodeDecodeEq xs = monadicIO $ do
  xs' <- run go
  assert $ xs == xs'
  where go = encode xs >>= decode

-- corrupt something, remove the last byte of the last bytestring
corrupt :: [ByteString] -> [ByteString]
corrupt = reverse . go . reverse
  where go (h:t) = ((S.reverse $ S.drop 1 $ S.reverse h):t)
        go [] = []

-- Encode something, corrupt the encoded data, and make sure we get a
-- decode error when we try do decode it.
encodeDecodeError :: forall a. (Serialize a,Eq a) => [a] -> Property
encodeDecodeError [] = monadicIO $ return ()
encodeDecodeError xs = monadicIO $ do
  run $ catch go $ \(_ :: DecodeException) -> return ()
  where go =
         do
           bList <- encode xs
           (xs' :: [a]) <- decode $ corrupt bList
           evaluate xs'
           fail "decoding succeeded when it should fail"

tests :: IO [Test]
tests =
 return [testProperty "encode-decode-equality Int"
         (encodeDecodeEq :: [Int] -> Property),
         testProperty "encode-decode-equality String"
         (encodeDecodeEq :: [String] -> Property),
         testProperty "encode-decode-equality Maybe Int"
         (encodeDecodeEq :: [Maybe Int] -> Property),
         testProperty "encode-decode-equality Either Int String"
         (encodeDecodeEq :: [Either Int String] -> Property),
         testProperty "encode-decode-equality (Int,Int)"
         (encodeDecodeEq :: [(Int,Int)] -> Property),
         testProperty "encode-decode-equality (String,String)"
         (encodeDecodeEq :: [(Int,Int)] -> Property),
         testProperty "encode-decode-error Int"
         (encodeDecodeError :: [Int] -> Property),
         testProperty "encode-decode-error String"
         (encodeDecodeError :: [String] -> Property),
         testProperty "encode-decode-error Maybe Int"
         (encodeDecodeError :: [Maybe Int] -> Property),
         testProperty "encode-decode-error Either Int String"
         (encodeDecodeError :: [Either Int String] -> Property),
         testProperty "encode-decode-error (Int,Int)"
         (encodeDecodeError :: [(Int,Int)] -> Property),
         testProperty "encode-decode-error (String,String)"
         (encodeDecodeError :: [(String,String)] -> Property)]
