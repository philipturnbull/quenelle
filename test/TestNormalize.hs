module TestNormalize (
    testNormalize
) where

import Language.Python.Common.AST
import Language.Python.Common.Pretty
import Language.Python.Version2.Parser
import Test.HUnit.Base

import Quenelle.Normalize

testRoundTrip :: String -> ExprSpan -> Test
testRoundTrip str e = TestCase $ assertEqual ("round-trip " ++ str) (prettyText e) (prettyText $ normalizeExpr e)

roundTrip str =
    case parseExpr str "" of
        Left _ -> TestCase $ assertFailure $ "Failed to parse: " ++ str
        Right (e, _) -> testRoundTrip str e

testNormalize :: Test
testNormalize = TestLabel "Normalize" $ TestList [
      r "0"
    , r "'0'"
    , r "(0)"
    , r "[0, 0]"
    , r "0,"
    , r "(0, 0)"
    , r "{0: 0}"
    , r "{0: (0)}"
    , r "f(0)"
    , r "f(0, 0)"
    , r "f(0, x=0)"
    , r "f(0, *[0], **{0:[0]})"
    , r "f([0])"
    , r "-(0)"
    , r "(0) + (0)"
    , r "(0) if (0) else (0)"
    , r "{0: 0}"
    -- This is only valid in python-2.7+
    --, r "{0, 0}"
    , r "[x for x in 0]"
    , r "[0 for x in [0]]"
    , r "[0 for x in [0] if 0]"
    , r "x[0]"
    , r "x[0:0]"
    , r "x[0:0:0]"
    , r "x[(0)]"
    , r "x[(0):(0)]"
    , r "x[(0):(0):(0)]"
    , r "0[(0):(0):(0)]"
    , r "(0)[(0):(0):(0)]"
    , r "lambda x: (0)"
    , r "lambda x, y: (0)"
    -- Yield is actually a statement
    --, r "yield (0, 0)"
    --, r "yield"
    , r "(0 for x in 0)"
    , r "(lambda x, y: 0)(0)"
    ]
    where r = roundTrip
