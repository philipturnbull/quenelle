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
    testDot,
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
    , t "x" "V1" "V1 + V1" "x + x"
    , t "x + y" "V1 + V2" "V2 + V1" "y + x"
    ]

testDot = testList "Dot" [
      t "x.y" "V1.V2" "V2.V1" "y.x"
    , t "x.y.z" "V1.V2.V3" "V3.V2.V1" "z.y.x"
    , t "x.y" "V1.E1" "V1.E1" "x.y"
    -- TODO: this seems to fail because exprToPred isn't doing the right
    -- thing for BoundExpressions on Dot expressions
    --, t "x.y" "V1.E1" "E1.V1" "y.x"
    ]

testParen = testList "Paren" [
      t "(0)" "0" "1" "(1)"
    , t "((0))" "0" "1" "((1))"
    , t "((x))" "(V1)" "V1" "(x)"
    ]

testBinaryOp = testList "BinaryOp" [
      t "(1 + 1)" "1 + 1" "2" "(2)"
    , t "(1 + 0)" "(E1 + 0)" "E1" "1"
    , t "(1 + 2)" "(E1 + E2)" "(E2 + E1)" "(2 + 1)"
    , t "(1 + 2)" "E1 + E2" "E2 + E1" "(2 + 1)"
    , t "(1 + (2))" "E1 + (E2)" "E1 + E2" "(1 + 2)"
    , t "(x + (y))" "(V1 + (V2))" "V1 + V2" "x + y"
    ]

testUnaryOp = testList "UnaryOp" [
      t "+0" "+E1" "E1" "0"
    , t "+0" "+E1" "-E1" "-0"
    , t "+x" "+V1" "V1" "x"
    , t "+x" "+V1" "-V1" "-x"
    ]

testCall = testList "Call" [
      t "f()" "E1()" "E1()" "f()"
    , t "f()" "E1()" "E1" "f"
    , t "f()" "V1()" "V1()" "f()"
    , t "f()" "V1()" "V1" "f"

    , t "f + g()" "E1 + E2()" "E2 + E1()" "g + f()"
    , t "f + g()" "E1 + E2()" "E1() + E2" "f() + g"

    , t "f + g()" "V1 + V2()" "V2 + V1()" "g + f()"
    , t "f + g()" "V1 + V2()" "V1() + V2" "f() + g"

    , t "f(g)" "E1(E2)" "E2(E1)" "g(f)"
    , t "f(x, y)" "f(E1, E2)" "f(E2, E1)" "f(y, x)"
    , t "f(x + y)" "f(E1 + E2)" "f(E1, E2)" "f(x, y)"
    , t "f(x, f(y + 0))" "E1 + 0" "E1" "f(x, f(y))"

    , t "f(g)" "V1(V2)" "V2(V1)" "g(f)"
    , t "f(x, y)" "f(V1, V2)" "f(V2, V1)" "f(y, x)"
    , t "f(x + y)" "f(V1 + V2)" "f(V1, V2)" "f(x, y)"
    , t "f(x, f(y + 0))" "V1 + 0" "V1" "f(x, f(y))"

    , t "f(*x)" "E1(*E2)" "E1(E2)" "f(x)"
    , t "f(*(x,))" "E1(*(E2,))" "E1(E2)" "f(x)"
    , t "f(*(x, y))" "E1(*(E2, E3))" "E1(E2, E3)" "f(x, y)"

    , t "f(*x)" "V1(*V2)" "V1(V2)" "f(x)"
    , t "f(*(x,))" "V1(*(V2,))" "V1(V2)" "f(x)"
    , t "f(*(x, y))" "V1(*(V2, V3))" "V1(V2, V3)" "f(x, y)"
    ]

testSubscript = testList "Subscript" [
      t "x[0]" "x[E1]" "E1" "0"
    , t "x[y]" "E1[E2]" "E2[E1]" "y[x]"
    , t "x[(y)]" "E1[(E2)]" "E2[E1]" "y[x]"

    , t "x[y]" "V1[V2]" "V2[V1]" "y[x]"
    , t "x[(y)]" "V1[(V2)]" "V2[V1]" "y[x]"
    ]

testTuple = testList "Tuple" [
      t "()" "E1" "E1" "()"
    , t "(x,)" "(E1,)" "(E1,)" "(x,)"
    , t "(x, y)" "(E1, E2)" "(E2, E1)" "(y, x)"

    , t "(x,)" "(V1,)" "(V1,)" "(x,)"
    , t "(x, y)" "(V1, V2)" "(V2, V1)" "(y, x)"
    ]

testListComp = testList "ListComp" [
      t "[x for x in xs]" "[E1 for E1 in E2]" "[E1 for E1 in E2]" "[x for x in xs]"
    , t "[f(x) for x in xs]" "[E1(E2) for E2 in E3]" "map(E1, E3)" "map(f, xs)"
    -- This isn't actually a valid transformation
    , t "[(x, y) for x in xs for y in ys]" "[E1 for E2 in E3 for E4 in E5]" "[E1 for (E2, E4) in zip(E3, E5)]" "[(x, y) for (x, y) in zip(xs, ys)]"

    , t "[f(x) for x in xs]" "[E1(E2) for E2 in E3]" "map(E1, E3)" "map(f, xs)"
    , t "[x for x in xs if f(x)]" "[E1 for E1 in E2 if E3(E1)]" "filter(E3, E2)" "filter(f, xs)"
    , t "[x for x in xs if x]" "[E1 for E1 in E2 if E1]" "filter(None, E2)" "filter(None, xs)"

    , t "[f(x) for x in xs]" "[V1(V2) for V2 in V3]" "map(V1, V3)" "map(f, xs)"
    , t "[x for x in xs if f(x)]" "[V1 for V1 in V2 if V3(V1)]" "filter(V3, V2)" "filter(f, xs)"
    , t "[x for x in xs if x]" "[V1 for V1 in V2 if V1]" "filter(None, V2)" "filter(None, xs)"
    ]
