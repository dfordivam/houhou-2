module Main where

import           Test.Tasty
import           Test.Tasty.Runners

import qualified HandlerTests as Handler
import DBInterface (openDB, close)

main :: IO ()
main = defaultMain (testGroup "houhou2-server tests"
  [WithResource (ResourceSpec openDB close) Handler.tests])
