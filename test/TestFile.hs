module TestFile (
    testFile
) where

import Data.Either
import Test.HUnit

import Quenelle.File

exprsLength (Left _) = -1
exprsLength (Right n) = length n

t strs n = TestCase $ assertEqual (show str) n $ exprsLength $ moduleExprsFromString "test.py" str
    where str = unlines strs

testList name tests = TestLabel name $ TestList tests

testFile :: Test
testFile = testList "moduleExprs" [
      testWhile
    , testFor
    , testAssign
    , testAugmentedAssign
    , testReturn
    , testWith
    , testDelete
    , testStmtExpr
    , testAssert
    , testPrint
    , testExec
    ]

testWhile = testList "While" [
      t ["while x: pass"] 1
    , t ["while x: y"] 2
    , t ["while x:", "  y", "else:", "  z"] 3
    ]

testFor = testList "For" [
      t ["for x in y: pass"] 2
    , t ["for x in y: z"] 3
    , t ["for x, y in z: pass"] 3
    , t ["for x in y:", " z", "else:", " z"] 4
    ]

testAssign = testList "Assign" [
      t ["x = y"] 2
    , t ["x, y = z"] 2 -- this is parsed as (Tuple = Var)
    , t ["x, y = z, w"] 2 -- this is parsed as (Tuple = Tuple)
    ]

testAugmentedAssign = testList "AugmentedAssign" [
      t ["x += y"] 2
    , t ["x += y, z"] 2
    ]

testReturn = testList "Return" [
      t ["return"] 0
    , t ["return x"] 1
    , t ["return x, y"] 1
    ]

testWith = testList "With" [
      t ["with x:", "  pass"] 1
    , t ["with x:", "  y"] 2
    , t ["with x:", "  x + y"] 2
    , t ["with x as y:", "  pass"] 2
    , t ["with x as y, z as w:", "  x + y"] 5
    ]

testDelete = testList "Delete" [
      t ["del x"] 1
    , t ["del x, y"] 2
    , t ["del x, y, z"] 3
    ]

testStmtExpr = testList "StmtExpr" [
      t ["x"] 1
    , t ["x", "y"] 2
    ]

testAssert = testList "Assert" [
      t ["assert x"] 1
    , t ["assert x, y"] 2
    , t ["assert x, y, z"] 3
    ]

testPrint = testList "Print" [
      t ["print"] 0
    , t ["print x"] 1
    , t ["print x,"] 1
    , t ["print x, y"] 2
    , t ["print >>f, x, y"] 3
    , t ["print >>(f, g), x, y"] 3
    ]

testExec = testList "Exec" [
      t ["exec x"] 1
    , t ["exec x in y"] 2
    , t ["exec x in y, z"] 3
    ]
