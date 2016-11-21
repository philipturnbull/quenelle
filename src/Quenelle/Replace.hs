{-# LANGUAGE ImpredicativeTypes #-}
module Quenelle.Replace (
    ExprReplacement(..),
    parseExprReplacement,
    doReplacement
) where

import Control.Exception.Base
import Control.Lens
import Data.Maybe
import Language.Python.Common.AST
import Language.Python.Common.ParseError
import Language.Python.Common.Pretty
import Language.Python.Version2.Parser

import Quenelle.Lens
import Quenelle.Match
import Quenelle.Rule
import Quenelle.Var

data ExprReplacement = ExprReplacement {
    exprReplacementExpr :: QExpr,
    exprReplacementBindings :: [RuleBinding]
    }

parseExprReplacement :: String -> Either ParseError ExprReplacement
parseExprReplacement str =
    case parseExprRule str of
        Left err -> Left err
        Right rule -> Right ExprReplacement {
            exprReplacementExpr = exprRuleExpr rule,
            exprReplacementBindings = exprRuleBindings rule
        }

doReplacement :: ExprReplacement -> ExprMatch -> QExpr
doReplacement replacement match = set matchPath boundReplacement matchExpr
    where matchPath = exprMatchPath match
          matchExpr = exprMatchExpr match
          boundReplacement = bindAllVars replacement match

bindAllVars :: ExprReplacement -> ExprMatch -> QExpr
bindAllVars replacement match = foldr (doBinding bound) replacementExpr unbound
    where replacementExpr = exprReplacementExpr replacement
          bound = exprMatchBindings match
          unbound = exprReplacementBindings replacement

doBinding :: [Binding] -> RuleBinding -> QExpr -> QExpr
doBinding bs (RuleVariableBinding name path) expr =
    case lookupVariableBinding bs name of
        Just val -> set path val expr
        Nothing -> expr
doBinding bs (RuleExpressionBinding name path) expr =
    case lookupExpressionBinding bs name of
        Just val -> set path val expr
        Nothing -> expr

lookupVariableBinding :: [Binding] -> VariableID -> Maybe QIdent
lookupVariableBinding [] name = Nothing
lookupVariableBinding ((VariableBinding iname i):bs) name
    | iname == name = Just i
    | otherwise = lookupVariableBinding bs name
lookupVariableBinding (_:bs) name = lookupVariableBinding bs name

lookupExpressionBinding :: [Binding] -> ExpressionID -> Maybe QExpr
lookupExpressionBinding [] name = Nothing
lookupExpressionBinding ((ExpressionBinding ename e):bs) name
    | ename == name = Just e
    | otherwise = lookupExpressionBinding bs name
lookupExpressionBinding (_:bs) name = lookupExpressionBinding bs name