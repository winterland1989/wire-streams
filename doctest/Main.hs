module Main
    ( main
    ) where

import Test.DocTest

main :: IO ()
main = doctest [
    "-isrc"
  , "-XOverloadedStrings"
  , "src/System/IO/Streams/Cereal.hs"
  ]

