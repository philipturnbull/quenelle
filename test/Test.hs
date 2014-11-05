module Main where

import Control.Monad
import System.Exit
import Test.HUnit

import TestMatch
import TestNormalize
import TestRule
import TestVar

tests = TestList [
    testMatch,
    testNormalize,
    testRule,
    testVar
    ]

main :: IO ()
main = do
    results <- runTestTT tests
    when (failures results > 0) exitFailure
