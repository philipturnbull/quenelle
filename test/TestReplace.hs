module TestReplace (
    testReplace
) where

import Data.List
import Language.Python.Common.AST
import Language.Python.Common.Pretty (prettyText)
import Language.Python.Version2.Parser
import Prelude hiding (fail)
import Test.HUnit.Base

import Quenelle.Match
import Quenelle.Normalize
import Quenelle.Replace
import Quenelle.Rule

testReplace :: Test
testReplace = TestLabel "doReplacement" $ TestList [
    testLiterals,
    testBindings,
    testParen,
    testBinaryOp,
    testUnaryOp,
    testCall,
    testSubscript,
    testTuple,
    testListComp
    ]

fail = TestCase . assertFailure

testDoReplacement sexpr srule sreplacement sexpected =
    case parseExpr sexpr "" of
        Left _ -> fail $ "Failed to parse sexpr: " ++ sexpr
        Right (expr, _) ->
            case parseExprRule srule of
                Left _ -> fail $ "Failed to parse srule: " ++ srule
                Right rule ->
                    case parseExpr sexpected "" of
                        Left _ -> fail $ "Failed to parse sexpected: " ++ sexpected
                        Right (expected, _) ->
                            case parseExprReplacement sreplacement of
                                Left _ -> fail $ "Failed to parse sreplacement: " ++ sreplacement
                                Right replacement ->
                                    case matchExprRule rule (normalizeExpr expr) of
                                        [] -> fail $ "Failed matchExprRule for: " ++ matchMsg
                                        [match] -> TestCase $ assertEqual assertString (normalizeExpr expected) (doReplacement replacement match)
                                        matches -> fail $ "Found " ++ (show $ length matches) ++ " matches for: " ++ matchMsg
    where matchMsg = srule ++ " -> " ++ sexpr
          assertString = intercalate " -> " [sexpr, srule, sreplacement, sexpected]

t = testDoReplacement

testList name tests = TestLabel name $ TestList tests

testLiterals = testList "literals" [
      t "0" "0" "1" "1"
    ]

testBindings = testList "bindings" [
      t "0" "E1" "E1" "0"
    , t "0" "E1" "E2" "E2" -- If a variable isn't bound, leave it alone
    ]

testParen = testList "Paren" [
      t "(0)" "0" "1" "(1)"
    , t "((0))" "0" "1" "((1))"
    ]

testBinaryOp = testList "BinaryOp" [
      t "(1 + 1)" "1 + 1" "2" "(2)"
    , t "(1 + 0)" "(E1 + 0)" "E1" "1"
    , t "(1 + 2)" "(E1 + E2)" "(E2 + E1)" "(2 + 1)"
    , t "(1 + 2)" "E1 + E2" "E2 + E1" "(2 + 1)"
    , t "(1 + (2))" "E1 + (E2)" "E1 + E2" "(1 + 2)"
    ]

testUnaryOp = testList "UnaryOp" [
      t "+0" "+E1" "E1" "0"
    , t "+0" "+E1" "-E1" "-0"
    ]

testCall = testList "Call" [
      t "f()" "E1()" "E1()" "f()"
    , t "f()" "E1()" "E1" "f"
    , t "f + g()" "E1 + E2()" "E2 + E1()" "g + f()"
    , t "f + g()" "E1 + E2()" "E1() + E2" "f() + g"

    , t "f(g)" "E1(E2)" "E2(E1)" "g(f)"
    , t "f(x, y)" "f(E1, E2)" "f(E2, E1)" "f(y, x)"
    , t "f(x + y)" "f(E1 + E2)" "f(E1, E2)" "f(x, y)"

    , t "f(*x)" "E1(*E2)" "E1(E2)" "f(x)"
    , t "f(*(x,))" "E1(*(E2,))" "E1(E2)" "f(x)"
    , t "f(*(x, y))" "E1(*(E2, E3))" "E1(E2, E3)" "f(x, y)"
    ]

testSubscript = testList "Subscript" [
      t "x[0]" "x[E1]" "E1" "0"
    , t "x[y]" "E1[E2]" "E2[E1]" "y[x]"
    , t "x[(y)]" "E1[(E2)]" "E2[E1]" "y[x]"
    ]

testTuple = testList "Tuple" [
      t "()" "E1" "E1" "()"
    , t "(x,)" "(E1,)" "(E1,)" "(x,)"
    , t "(x, y)" "(E1, E2)" "(E2, E1)" "(y, x)"
    ]

testListComp = testList "ListComp" [
      t "[x for x in xs]" "[E1 for E1 in E2]" "[E1 for E1 in E2]" "[x for x in xs]"
    , t "[f(x) for x in xs]" "[E1(E2) for E2 in E3]" "map(E1, E3)" "map(f, xs)"
    -- This isn't actually a valid transformation
    , t "[(x, y) for x in xs for y in ys]" "[E1 for E2 in E3 for E4 in E5]" "[E1 for (E2, E4) in zip(E3, E5)]" "[(x, y) for (x, y) in zip(xs, ys)]"

    , t "[f(x) for x in xs]" "[E1(E2) for E2 in E3]" "map(E1, E3)" "map(f, xs)"
    ]
