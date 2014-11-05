module TestVar (
    testVar
) where

import Control.Applicative
import Data.List
import Language.Python.Common.AST
import Test.HUnit.Base

import Quenelle.Var

testVar = TestList [
    testClassifyVar,
    testValidateVars
    ]

testClassifyVar :: Test
testClassifyVar = TestLabel "classifyVar" $ TestList [
      "E" ~> (Expression, "E")
    , "E1" ~> (BoundExpression, "E1")
    , "E32" ~> (BoundExpression, "E32")
    , "V" ~> (Variable, "V")
    , "x" ~> (Normal, "x")
    , "foo" ~> (Normal, "foo")
    ]
    where (~>) ident expected = TestCase $ assertEqual ident (classifyVar (Ident ident ())) expected

testValidateVars :: Test
testValidateVars = TestLabel "validateVars" $ TestList [
      [] ~> Just []
    , [e1_1] ~> Just [e1_1]
    , [e2_1] ~> Just [e2_1]
    , [e1_1, e2_1] ~> Just [e1_1, e2_1]
    , [e1_2, e2_2] ~> Just [e1_2, e2_2]
    , [e1_1, e1_2] ~> Nothing
    , [e2_1, e2_2] ~> Nothing
    ]
    where (~>) :: [(String, Int)] -> Maybe [(String, Int)] -> Test
          (~>) vars expected = TestCase $ assertEqual (show vars) (sort <$> expected) (sort <$> validateVars vars)
          e1_1 = ("E1", 1)
          e1_2 = ("E1", 2)
          e2_1 = ("E2", 1)
          e2_2 = ("E2", 2)
