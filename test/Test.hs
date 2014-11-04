module Main where

import Control.Monad
import System.Exit
import Test.HUnit

import Normalize

tests = TestList [
    testNormalize
    ]

main :: IO ()
main = do
    results <- runTestTT tests
    when (failures results > 0) exitFailure
