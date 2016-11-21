{-# LANGUAGE Rank2Types #-}
module TestRule (
    testRule
) where

import Control.Lens
import Data.Maybe
import Language.Python.Common.AST
import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST
import Language.Python.Version2.Parser
import Test.HUnit.Base

import Quenelle.Lens
import Quenelle.Normalize
import Quenelle.Rule

go :: ExprSpan -> ExprRule -> String
go e r = (show name) ++ ": " ++ (prettyText $ fromJust $ (normalizeExpr e) ^? path) ++ " "
    where (RuleVariableBinding name path) = head $ exprRuleBindings r

matchRule b rule str =
    case parseExpr str "" of
        Left _ -> TestCase $ assertFailure $ "Failed to parse: " ++ str
        Right (e, _) ->
            case parseExprRule rule of
                Left _ -> TestCase $ assertFailure $ "Failed to parse rule: " ++ rule
                Right r -> TestCase $ assertEqual ((go e r) ++ rule ++ " -> " ++ str) b ((exprRulePred r) (normalizeExpr e))

testRule :: Test
testRule = TestLabel "Rule" $ TestList [
      t "0" "0"
    , f "0" "10"
    , t "(0)" "(0)"
    , t "((0))" "((0))"
    , f "(0)" "0"
    , t "0 + 1" "0 + 1"
    , f "0 + 1" "1 + 0"
    , t "x" "x"
    , f "x" "y"
    , t "E" "1"
    , t "E + E" "1 + 1"
    , t "E + E" "(1) + (1)"
    , t "E1" "1"
    , t "E1 + E2" "1 + 2"

    , t "E1 + E1" "1 + 1"
    -- exprRulePred doesn't validate that the same variable is bound to different values
    , t "E1 + E1" "1 + 2"
    ]
    where t = matchRule True
          f = matchRule False
