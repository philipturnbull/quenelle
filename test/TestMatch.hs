module TestMatch (
    testMatch
) where

import Data.List
import Language.Python.Common.AST
import Language.Python.Common.Pretty
import Language.Python.Version2.Parser
import Test.HUnit.Base

import Quenelle.Match
import Quenelle.Normalize
import Quenelle.Rule

testMatch = TestList [
    testMatchExprRule
    ]

allMatchBindings matches = map exprMatchBindings matches

assertVars rulestr exprstr expected matches =
    TestCase $ assertEqual (rulestr ++ " -> " ++ exprstr) (sort expected) (sort $ allMatchBindings matches)

testRule rulestr exprstr expected =
    case parseExprRule rulestr of
        Left _ -> TestCase $ assertFailure $ "Failed to parse rule: " ++ rulestr
        Right rule -> case parseExpr exprstr "" of
                        Left _ -> TestCase $ assertFailure $ "Failed to parse expr: " ++ exprstr
                        Right (expr, _) -> assertVars rulestr exprstr expected $ matchExprRule rule (normalizeExpr expr)


testMatchExprRule :: Test
testMatchExprRule = TestLabel "matchExprRule" $ TestList [
      t "0" "0" [[]]
    , t "0" "1" []
    , t "0L" "0L" [[]]

    , t "E1" "0" [[("E1", zero)]]
    , t "(E1)" "(0)" [[("E1", zero)]]
    , t "(E1)" "0 + 1" []
    , t "E1" "((0))" [[("E1", parens $ parens zero)], [("E1", parens zero)], [("E1", zero)]]
    , t "E1 + E2" "0 + 1" [[("E1", zero), ("E2", one)]]
    , t "E1 + E1" "0 + 0" [[("E1", zero)]]
    , t "-E1" "-1" [[("E1", one)]]
    , t "-E1" "(-0) + (-1)" [[("E1", zero)], [("E1", one)]]
    , t "E1()" "f()" [[("E1", var "f")]]
    , t "E1()" "0()" [[("E1", zero)]]
    ]
    where t = testRule
          zero = Int 0 "0" ()
          one = Int 1 "1" ()
          add l r = BinaryOp (Plus ()) l r ()
          parens e = Paren e ()
          var s = Var (Ident s ()) ()
