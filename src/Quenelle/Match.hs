{-# LANGUAGE ImpredicativeTypes, Rank2Types #-}
module Quenelle.Match (
    ExprMatch(..),
    matchExprRule
) where

import Control.Lens
import Control.Lens.At
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

childExprs path e@(Call fun args _) = (path, e) : (childExprs (path.call_funL) fun) ++ (argumentsChildExprs path args)

childExprs path e@(Subscript s se _) =
    (path, e) : (childExprs (path.subscripteeL) s) ++ (childExprs (path.subscript_exprL) se)

childExprs path e@(Paren pe _) =
    (path, e) : (childExprs (path.paren_exprL) pe)

childExprs path e@(Tuple es _) =
    (path, e) : (tupleChildExprs (path.tuple_exprsL) es)

argumentsChildExprs :: QPath -> [QArgument] -> [(QPath, QExpr)]
argumentsChildExprs path args = [argumentChildExpr (path.call_argsL.(ix i)) arg | (arg, i) <- zip args [0..]]

argumentChildExpr :: (Traversal' QExpr QArgument) -> QArgument -> (QPath, QExpr)
argumentChildExpr path (ArgExpr e _) = (path.arg_exprL, e)

tupleChildExprs :: (Traversal' QExpr [QExpr]) -> [QExpr] -> [(QPath, QExpr)]
tupleChildExprs path es = concat [childExprs (path.(ix i)) e | (e, i) <- zip es [0..]]
