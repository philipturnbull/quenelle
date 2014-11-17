module TestFile (
    testFile
) where

import Data.Either
import Test.HUnit

import Quenelle.File

exprsLength (Left _) = -1
exprsLength (Right n) = length n

t str n = TestCase $ assertEqual (show str) n $ exprsLength $ moduleExprsFromString "test.py" str

testFile :: Test
testFile = TestLabel "moduleExprs" $ TestList [
      t "" 0
    , t "x\n" 1
    , t "x\ny\n" 2
    ]
