module Main where

import Control.Monad
import System.Exit
import Test.HUnit

import TestMatch
import TestNormalize
import TestReplace
import TestRule
import TestVar

tests = TestList [
    testMatch,
    testNormalize,
    testReplace,
    testRule,
    testVar
    ]

main :: IO ()
main = do
    results <- runTestTT tests
    when (failures results > 0 || errors results > 0) exitFailure
