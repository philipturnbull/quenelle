module TestMatch (
    testMatch
) where

import Data.List
import Language.Python.Common.AST
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Version2.Parser
import Test.HUnit.Base

import Quenelle.Lens
import Quenelle.Match
import Quenelle.Normalize
import Quenelle.Replace
import Quenelle.Rule

import QuickCheck

testMatch = TestList [
      testMatchExprRule
    , quickCheckMatchExprRule
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

t = testRule
zero = Int 0 "0" ()
one = Int 1 "1" ()
two = Int 2 "2" ()
three = Int 3 "3" ()
add l r = BinaryOp (Plus ()) l r ()
parens e = Paren e ()
var s = Var (Ident s ()) ()

testList name tests = TestLabel name $ TestList tests

testMatchExprRule :: Test
testMatchExprRule = testList "matchExprRule" [
      testIntegers
    , testParen
    , testBinaryOp
    , testUnaryOp
    , testSubscript
    , testCall
    ]

testIntegers = testList "Int/LongInt" [
      t "0" "0" [[]]
    , t "0" "1" []
    , t "0" "0x00000000" [[]]
    , t "8" "010" [[]]
    , t "0L" "0L" [[]]
    , t "E1" "0" [[("E1", zero)]]
    ]

testParen = testList "Paren" [
      t "(E1)" "(0)" [[("E1", zero)]]
    , t "(E1)" "0 + 1" []
    , t "E1" "((0))" [[("E1", parens $ parens zero)], [("E1", parens zero)], [("E1", zero)]]
    ]

testBinaryOp = testList "BinaryOp" [
      t "E1 + E2" "0 + 1" [[("E1", zero), ("E2", one)]]
    , t "E1 + E1" "0 + 0" [[("E1", zero)]]
    ]

testUnaryOp = testList "UnaryOp" [
      t "-E1" "-1" [[("E1", one)]]
    , t "-E1" "(-0) + (-1)" [[("E1", zero)], [("E1", one)]]
    ]

testSubscript = testList "Subscript" [
      t "E1[E1]" "0[0]" [[("E1", zero)]]
    , t "E1[E2]" "0[1]" [[("E1", zero), ("E2", one)]]
    , t "E1[E1]" "0[1]" []
    ]

testCall = testList "Call" [
      t "E1()" "f()" [[("E1", var "f")]]
    , t "E1()" "0()" [[("E1", zero)]]
    ]

quickCheckMatchExprRule = qc count_matches 10000 "quickCheckMatchExprRule"

-- The Expr Arbitrary instance generates (Var "vvv ()) nodes. Convert the expr
-- to a string and count the number of "vvv" substrings. This should match the
-- number of matches found by matchExprRule. This mostly tests that Quenelle.Match.childExprs
-- correctly recuses to child expressions.
count_matches :: QExpr -> Bool
count_matches expr = count_vvv 0 (prettyText expr) == length (matchExprRule rule expr)
    where (Right rule) = parseExprRule "vvv"
          count_vvv acc [] = acc
          count_vvv acc ('v':'v':'v':str) = count_vvv (acc + 1) str
          count_vvv acc (_:str) = count_vvv acc str
