module TestMatch (
    testMatch
) where

import Data.List
import Language.Python.Common.AST
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Common.PrettyAST
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
      testNumbers
    , testStrings
    , testVariableBinding
    , testParen
    , testBinaryOp
    , testUnaryOp
    , testSubscript
    , testCall
    , testLambda
    , testCondExpr
    , testSlicedExpr
    , testStringConversion
    ]

testNumbers = testList "numbers" [
      t "0" "0" [[]]
    , t "0" "1" []
    , t "0" "0x00000000" [[]]
    , t "8" "010" [[]]
    , t "0L" "0L" [[]]
    , t "E1" "0" [[("E1", zero)]]
    , t "1.0" "1.0" [[]]
    , t "1j" "1j" [[]]
    ]

testStrings = testList "strings" [
      t "'abc'" "'abc'" [[]]
    , t "\"abc\"" "\"abc\"" [[]]
    , t "'a' 'a'" "'a' 'a'" [[]]

    , t "b'abc'" "b'abc'" [[]]
    , t "b\"abc\"" "b\"abc\"" [[]]
    , t "b'a' b'a'" "b'a' b'a'" [[]]

    , t "u'abc'" "u'abc'" [[]]
    , t "u\"abc\"" "u\"abc\"" [[]]
    , t "u'a' u'a'" "u'a' u'a'" [[]]
    ]

testVariableBinding = testList "variables" [
      t "V1" "x" [[("V1", var "x")]]
    , t "V1" "foo" [[("V1", var "foo")]]
    , t "x.V2" "x.y" [[("V2", var "y")]]
    , t "V1.y" "x.y" [[("V1", var "x")]]
    , t "V1.V2" "x.y" [[("V1", var "x"), ("V2", var "y")]]
    , t "x.V2.V3" "x.y.z" [[("V2", var "y"), ("V3", var "z")]]
    , t "V1.y.V3" "x.y.z" [[("V1", var "x"), ("V3", var "z")]]
    , t "V1.V2.z" "x.y.z" [[("V1", var "x"), ("V2", var "y")]]
    , t "V1.V2.V3" "x.y.z" [[("V1", var "x"), ("V2", var "y"), ("V3", var "z")]]
    ]

testParen = testList "Paren" [
      t "(E1)" "(0)" [[("E1", zero)]]
    , t "(E1)" "0 + 1" []
    , t "E1" "((0))" [[("E1", parens $ parens zero)], [("E1", parens zero)], [("E1", zero)]]

    , t "V1" "((x))" [[("V1", var "x")]]
    ]

testBinaryOp = testList "BinaryOp" [
      t "E1 + E2" "0 + 1" [[("E1", zero), ("E2", one)]]
    , t "E1 + E1" "0 + 0" [[("E1", zero)]]

    , t "V1 + V1" "x + x" [[("V1", var "x")]]
    , t "V1 + V1" "x + y" []
    ]

testUnaryOp = testList "UnaryOp" [
      t "-E1" "-1" [[("E1", one)]]
    , t "-E1" "(-0) + (-1)" [[("E1", zero)], [("E1", one)]]

    , t "-V1" "-x" [[("V1", var "x")]]
    ]

testSubscript = testList "Subscript" [
      t "E1[E1]" "0[0]" [[("E1", zero)]]
    , t "E1[E2]" "0[1]" [[("E1", zero), ("E2", one)]]
    , t "E1[E1]" "0[1]" []
    -- x[y, z] seems ambiguous. It is parsed as "Subscript[Tuple]" but it
    -- is possible to create a "SlicedExpr[SliceExpr, SliceExpr]" which serialises
    -- to the same form.
    , t "E1[E2, E3]" "0[1, 2]" [[("E1", zero), ("E2", one), ("E3", two)]]

    , t "V1[V2]" "x[y]" [[("V1", var "x"), ("V2", var "y")]]
    , t "V1[V1]" "x[y]" []
    ]

testCall = testList "Call" [
      t "E1()" "f()" [[("E1", var "f")]]
    , t "E1()" "0()" [[("E1", zero)]]

    , t "V1()" "f()" [[("V1", var "f")]]
    ]

testLambda = testList "Lambda" [
      t "lambda: E1" "lambda: 0" [[("E1", zero)]]
    , t "lambda: lambda: E1" "lambda: lambda: 0" [[("E1", zero)]]

    , t "lambda: V1" "lambda: x" [[("V1", var "x")]]
    , t "lambda: lambda: V1" "lambda: lambda: x" [[("V1", var "x")]]
    ]

testCondExpr = testList "CondExpr" [
      t "E1 if E2 else E3" "0 if 1 else 2" [[("E1", zero), ("E2", one), ("E3", two)]]

    , t "V1 if V2 else V3" "x if y else z" [[("V1", var "x"), ("V2", var "y"), ("V3", var "z")]]
    ]

testSlicedExpr = testList "SlicedExpr" [
      t "E1[:]" "0[:]" [[("E1", zero)]]
    , t "E1[E2:]" "0[1:]" [[("E1", zero), ("E2", one)]]
    , t "E1[E2:E3]" "0[1:2]" [[("E1", zero), ("E2", one), ("E3", two)]]
    , t "E1[E2:E3:]" "0[1:2:]" [[("E1", zero), ("E2", one), ("E3", two)]]
    , t "E1[E2:E3:E4]" "0[1:2:3]" [[("E1", zero), ("E2", one), ("E3", two), ("E4", three)]]

    , t "V1[V2:V3:V4]" "w[x:y:z]" [[("V1", var "w"), ("V2", var "x"), ("V3", var "y"), ("V4", var "z")]]
    ]

testStringConversion = testList "StringConversion" [
      t "`E1`" "`0`" [[("E1", zero)]]

    , t "`V1`" "`x`" [[("V1", var "x")]]
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
