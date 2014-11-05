{-# LANGUAGE ImpredicativeTypes, Rank2Types #-}
module Quenelle.Match (
    ExprMatch(..),
    matchExprRule
) where

import Control.Monad.State.Strict
import Data.List
import Data.Maybe

import Language.Python.Common.AST

import Quenelle.Lens
import Quenelle.Normalize
import Quenelle.Rule
import Quenelle.Var

data ExprMatch = ExprMatch {
    exprMatchRule :: ExprRule,
    exprMatchExpr :: QExpr,
    exprMatchPath :: QPath,
    exprMatchBindings :: [(String, QExpr)]
    }

matchExprRule :: ExprRule -> QExpr -> [ExprMatch]
matchExprRule rule expr = mapMaybe (matchChildExpr rule expr) (childExprs id expr)

matchChildExpr :: ExprRule -> QExpr -> (QPath, QExpr) -> Maybe ExprMatch
matchChildExpr rule root (path, expr) = do
    raw_vars <- runExprRule rule expr
    vars <- validateVars raw_vars
    return $ ExprMatch {
        exprMatchRule = rule,
        exprMatchExpr = root,
        exprMatchPath = path,
        exprMatchBindings = vars
    }

childExprs :: QPath -> QExpr -> [(QPath, QExpr)]
childExprs path e@Var{} = [(path, e)]
childExprs path e@Int{} = [(path, e)]
childExprs path e@LongInt{} = [(path, e)]
childExprs path e@Bool{} = [(path, e)]
childExprs path e@(BinaryOp _ l r _) =
    (path, e) : (childExprs (path.left_op_argL) l) ++ (childExprs (path.right_op_argL) r)
childExprs path e@(UnaryOp _ ue _) = (path, e) : (childExprs (path.op_argL) ue)

childExprs path e@(Call fun [] _) = (path, e) : (childExprs (path.call_funL) fun)

childExprs path e@(Subscript s se _) =
    (path, e) : (childExprs (path.subscripteeL) s) ++ (childExprs (path.subscript_exprL) se)

childExprs path e@(Paren pe _) =
    (path, e) : (childExprs (path.paren_exprL) pe)
