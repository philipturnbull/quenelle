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
      "E" ~> Expression
    , "E1" ~> (BoundExpression $ ExpressionID 1)
    , "E32" ~> (BoundExpression $ ExpressionID 32)
    , "V" ~> Variable
    , "V1" ~> (BoundVariable $ VariableID 1)
    , "V99" ~> (BoundVariable $ VariableID 99)
    , "x" ~> Normal
    , "foo" ~> Normal
    , "Efoo" ~> Normal
    , "Vfoo" ~> Normal
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
    where (~>) :: [Binding] -> Maybe [Binding] -> Test
          (~>) vars expected = TestCase $ assertEqual (show vars) (sort <$> expected) (sort <$> validateBindings vars)
          e1_1 = (ExpressionBinding (ExpressionID 1) (Int 1 "1" ()))
          e1_2 = (ExpressionBinding (ExpressionID 1) (Int 1 "2" ()))
          e2_1 = (ExpressionBinding (ExpressionID 2) (Int 1 "1" ()))
          e2_2 = (ExpressionBinding (ExpressionID 2) (Int 1 "2" ()))
