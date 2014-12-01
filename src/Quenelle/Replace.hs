{-# LANGUAGE ImpredicativeTypes #-}
module Quenelle.Replace (
    ExprReplacement(..),
    parseExprReplacement,
    doReplacement
) where

import Control.Exception.Base
import Control.Lens
import Language.Python.Common.AST
import Language.Python.Common.ParseError
import Language.Python.Common.Pretty
import Language.Python.Version2.Parser

import Quenelle.Lens
import Quenelle.Match
import Quenelle.Rule

data ExprReplacement = ExprReplacement {
    exprReplacementExpr :: QExpr,
    exprReplacementUnboundVars :: [(String, QExprPath)]
    }

parseExprReplacement :: String -> Either ParseError ExprReplacement
parseExprReplacement str =
    case parseExprRule str of
        Left err -> Left err
        Right rule -> Right ExprReplacement {
            exprReplacementExpr = exprRuleExpr rule,
            exprReplacementUnboundVars = exprRuleBoundVars rule
        }

doReplacement :: ExprReplacement -> ExprMatch -> QExpr
doReplacement replacement match = set matchPath boundReplacement matchExpr
    where matchPath = exprMatchPath match
          matchExpr = exprMatchExpr match
          boundReplacement = bindAllVars replacement match

bindAllVars :: ExprReplacement -> ExprMatch -> QExpr
bindAllVars replacement match = foldr (bindVar boundVars) replacementExpr unboundVars
    where replacementExpr = exprReplacementExpr replacement
          boundVars = exprMatchBindings match
          unboundVars = exprReplacementUnboundVars replacement

bindVar :: [(String, QExpr)] -> (String, QExprPath) -> QExpr -> QExpr
bindVar vars (name, path) expr =
    case lookup name vars of
        Just val -> set path val expr
        Nothing -> set path (Var (Ident name ()) ()) expr
