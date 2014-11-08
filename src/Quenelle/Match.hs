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
    return ExprMatch {
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
    (path, e) : childExprs (path.left_op_argL) l ++ childExprs (path.right_op_argL) r
childExprs path e@(UnaryOp _ ue _) = (path, e) : childExprs (path.op_argL) ue

childExprs path e@(Call fun args _) = (path, e) : childExprs (path.call_funL) fun ++ argumentsChildExprs path args

childExprs path e@(Subscript s se _) =
    (path, e) : childExprs (path.subscripteeL) s ++ childExprs (path.subscript_exprL) se

childExprs path e@(Paren pe _) =
    (path, e) : childExprs (path.paren_exprL) pe

childExprs path e@(Tuple es _) =
    (path, e) : concatChildExprs (path.tuple_exprsL) es

childExprs path e@(ListComp comp _) =
    (path, e) : listCompExprChildExprs (path.list_comprehensionL) comp


argumentsChildExprs :: QPath -> [QArgument] -> [(QPath, QExpr)]
argumentsChildExprs path args = [argumentChildExpr (path.call_argsL.ix i) arg | (arg, i) <- zip args [0..]]

argumentChildExpr :: Traversal' QExpr QArgument -> QArgument -> (QPath, QExpr)
argumentChildExpr path (ArgExpr e _) = (path.arg_exprL, e)
argumentChildExpr path (ArgVarArgsPos e _) = (path.arg_exprL, e)

listCompExprChildExprs :: Traversal' QExpr (QComprehension QExpr) -> QComprehension QExpr -> [(QPath, QExpr)]
listCompExprChildExprs path (Comprehension e for _) =
    childExprs (path.comprehension_exprL) e ++ compForChildExprs (path.comprehension_forL) for

compForChildExprs :: Traversal' QExpr QCompFor -> QCompFor -> [(QPath, QExpr)]
compForChildExprs path (CompFor fores ine iter _) =
    concatChildExprs (path.comp_for_exprsL) fores ++ childExprs (path.comp_in_exprL) ine ++ compIterChildExprs (path.comp_for_iterL) iter

compIfChildExprs :: Traversal' QExpr QCompIf -> QCompIf -> [(QPath, QExpr)]
compIfChildExprs path (CompIf if_ iter _) =
    childExprs (path.comp_ifL) if_ ++ compIterChildExprs (path.comp_if_iterL) iter

compIterChildExprs :: Traversal' QExpr (Maybe QCompIter) -> Maybe QCompIter -> [(QPath, QExpr)]
compIterChildExprs path (Just (IterFor for _)) = compForChildExprs (path._Just.comp_iter_forL) for
compIterChildExprs path (Just (IterIf if_ _)) = compIfChildExprs (path._Just.comp_iter_ifL) if_
compIterChildExprs path Nothing = []

concatChildExprs :: Traversal' QExpr [QExpr] -> [QExpr] -> [(QPath, QExpr)]
concatChildExprs path es = concat [childExprs (path.ix i) e | (e, i) <- zip es [0..]]
