module TestParse (
    testParse
) where

import Data.Either
import Test.HUnit

import Quenelle.Parse

isSuccess (Success _ _) = True
isSuccess _ = False

t rule = TestCase $ assertBool ("Failed to parse: " ++ show rule) (isSuccess $ parseRuleFromString "rule" rule)
f rule = TestCase $ assertBool ("Successfully parsed: " ++ show rule) (not $ isSuccess $ parseRuleFromString "rule" rule)

testParse :: Test
testParse = TestLabel "parseRuleFromString" $ TestList [
      t "replace:\nx\nwith:\ny\n"

    , f "replace\nx\nwith:\ny\n"
    , f "replace:x\nwith:\ny\n"
    , f "replace:\n\nwith:\ny\n"
    , f "replace:\nx\nwith\ny\n"
    , f "replace:\nx\nwith:y\n"
    , f "replace:\nx\nwith:\n\n"
    , f "replace:\nx\nwith:\ny"
    ]
