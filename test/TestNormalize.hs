module TestNormalize (
    testNormalize
) where

import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST
import Test.HUnit.Base

import Quenelle.Normalize

import QuickCheck

testNormalize :: Test
testNormalize = TestLabel "Normalize" $ qc roundTrip 10000 "QuickCheckNormalize"
    where roundTrip e = prettyText e == prettyText (normalizeExpr e)
