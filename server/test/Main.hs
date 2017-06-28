module Main where

import           Test.Tasty

import qualified HandlerTests as Handler

main :: IO ()
main = defaultMain (testGroup "houhou2-server tests"
                              [ Handler.tests])
