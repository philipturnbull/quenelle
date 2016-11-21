{-# LANGUAGE ImpredicativeTypes, Rank2Types #-}
module Quenelle.Match (
    ExprMatch(..),
    matchExprRule
) where

import Control.Lens hiding (List)
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
    exprMatchPath :: QExprPath,
    exprMatchBindings :: [Binding]
    }

matchExprRule :: ExprRule -> QExpr -> [ExprMatch]
matchExprRule rule expr = mapMaybe (matchChildExpr rule expr) (childExprs id expr)

matchChildExpr :: ExprRule -> QExpr -> (QExprPath, QExpr) -> Maybe ExprMatch
matchChildExpr rule root (path, expr) = do
    raw_bindings <- runExprRule rule expr
    bindings <- validateBindings raw_bindings
    return ExprMatch {
        exprMatchRule = rule,
        exprMatchExpr = root,
        exprMatchPath = path,
        exprMatchBindings = bindings
    }

childExprs :: QExprPath -> QExpr -> [(QExprPath, QExpr)]
childExprs path e@Var{} = [(path, e)]
childExprs path e@Int{} = [(path, e)]
childExprs path e@LongInt{} = [(path, e)]
childExprs path e@Float{} = [(path, e)]
childExprs path e@Imaginary{} = [(path, e)]
childExprs path e@Bool{} = [(path, e)]
childExprs path e@ByteStrings{} = [(path, e)]
childExprs path e@Strings{} = [(path, e)]
childExprs path e@UnicodeStrings{} = [(path, e)]
childExprs path e@(BinaryOp _ l r _) =
    (path, e) : childExprs (path.left_op_argL) l ++ childExprs (path.right_op_argL) r
childExprs path e@(UnaryOp _ ue _) = (path, e) : childExprs (path.op_argL) ue

childExprs path e@(Dot de _ _) = (path, e) : childExprs (path.dot_exprL) de

childExprs path e@(Lambda args body _) =
    (path, e) : paramsChildExprs (path.lambda_argsL) args ++ childExprs (path.lambda_bodyL) body

childExprs path e@(Call fun args _) = (path, e) : childExprs (path.call_funL) fun ++ argumentsChildExprs path args

childExprs path e@(Subscript s se _) =
    (path, e) : childExprs (path.subscripteeL) s ++ childExprs (path.subscript_exprL) se

childExprs path e@(CondExpr t c f _) =
    (path, e) : childExprs (path.ce_true_branchL) t ++
        childExprs (path.ce_conditionL) c ++
            childExprs (path.ce_false_branchL) f

childExprs path e@(SlicedExpr s ss _) =
    (path, e) : childExprs (path.sliceeL) s ++ concatSliceChildExprs (path.slicesL) ss

childExprs path e@(Paren pe _) =
    (path, e) : childExprs (path.paren_exprL) pe

childExprs path e@(StringConversion qe _) =
    (path, e) : childExprs (path.backquoted_exprL) qe

childExprs path e@(Tuple es _) =
    (path, e) : concatChildExprs (path.tuple_exprsL) es

childExprs path e@(Yield ye _) =
    (path, e) : maybeYieldArg (path.yield_argL) ye

childExprs path e@(Generator comp _) =
    (path, e) : compExprChildExprs (path.gen_comprehensionL) comp

childExprs path e@(ListComp comp _) =
    (path, e) : compExprChildExprs (path.list_comprehensionL) comp

childExprs path e@(List es _) =
    (path, e) : concatChildExprs (path.list_exprsL) es

childExprs path e@(Dictionary kvs _) =
    (path, e) : concatDictChildExprs (path.dict_mappingsL) kvs

childExprs path e@(Set es _) =
    (path, e) : concatChildExprs (path.set_exprsL) es

argumentsChildExprs :: QExprPath -> [QArgument] -> [(QExprPath, QExpr)]
argumentsChildExprs path args = concat [argumentChildExprs (path.call_argsL.ix i) arg | (arg, i) <- zip args [0..]]

argumentChildExprs :: Traversal' QExpr QArgument -> QArgument -> [(QExprPath, QExpr)]
argumentChildExprs path (ArgExpr e _) = childExprs (path.arg_exprL) e
argumentChildExprs path (ArgVarArgsPos e _) = childExprs (path.arg_exprL) e
argumentChildExprs path (ArgVarArgsKeyword e _) = childExprs (path.arg_exprL) e
argumentChildExprs path (ArgKeyword _ e _) = childExprs (path.arg_exprL) e

compExprChildExprs :: Traversal' QExpr QComprehension -> QComprehension -> [(QExprPath, QExpr)]
compExprChildExprs path (Comprehension e for _) =
    comprehensionExprExprs (path.comprehension_exprL) e ++ compForChildExprs (path.comprehension_forL) for

comprehensionExprExprs :: Traversal' QExpr QComprehensionExpr -> QComprehensionExpr -> [(QExprPath, QExpr)]
comprehensionExprExprs path (ComprehensionExpr e) = childExprs (path._ComprehensionExpr) e
comprehensionExprExprs path (ComprehensionDict dmp) = dictChildExprs (path._ComprehensionDict) dmp

compForChildExprs :: Traversal' QExpr QCompFor -> QCompFor -> [(QExprPath, QExpr)]
compForChildExprs path (CompFor fores ine iter _) =
    concatChildExprs (path.comp_for_exprsL) fores ++ childExprs (path.comp_in_exprL) ine ++ compIterChildExprs (path.comp_for_iterL) iter

compIfChildExprs :: Traversal' QExpr QCompIf -> QCompIf -> [(QExprPath, QExpr)]
compIfChildExprs path (CompIf if_ iter _) =
    childExprs (path.comp_ifL) if_ ++ compIterChildExprs (path.comp_if_iterL) iter

compIterChildExprs :: Traversal' QExpr (Maybe QCompIter) -> Maybe QCompIter -> [(QExprPath, QExpr)]
compIterChildExprs path (Just (IterFor for _)) = compForChildExprs (path._Just.comp_iter_forL) for
compIterChildExprs path (Just (IterIf if_ _)) = compIfChildExprs (path._Just.comp_iter_ifL) if_
compIterChildExprs path Nothing = []

concatChildExprs :: Traversal' QExpr [QExpr] -> [QExpr] -> [(QExprPath, QExpr)]
concatChildExprs path es = concat [childExprs (path.ix i) e | (e, i) <- zip es [0..]]

concatDictChildExprs :: Traversal' QExpr [QDictMappingPair] -> [QDictMappingPair] -> [(QExprPath, QExpr)]
concatDictChildExprs path kvs =
    concat [dictChildExprs (path.ix i) dmp | (dmp, i) <- ikvs]
    where ikvs = zip kvs [0..]

dictChildExprs :: Traversal' QExpr QDictMappingPair -> QDictMappingPair -> [(QExprPath, QExpr)]
dictChildExprs path (DictMappingPair k v) = childExprs (path._DictMappingPair._1) k ++ childExprs (path._DictMappingPair._2) v

concatSliceChildExprs :: Traversal' QExpr [QSlice] -> [QSlice] -> [(QExprPath, QExpr)]
concatSliceChildExprs path ss = concat [sliceChildExprs (path.ix i) s | (s, i) <- zip ss [0..]]

sliceChildExprs :: Traversal' QExpr QSlice -> QSlice -> [(QExprPath, QExpr)]
sliceChildExprs path (SliceProper l u s _) =
    maybeChildExprs (path.slice_lowerL) l ++
        maybeChildExprs (path.slice_upperL) u ++
            maybe [] (maybeChildExprs (path.slice_strideL._Just)) s
sliceChildExprs path (SliceExpr e _) = childExprs (path.slice_exprL) e
sliceChildExprs path (SliceEllipsis _) = []

maybeChildExprs :: Traversal' QExpr (Maybe QExpr) -> Maybe QExpr -> [(QExprPath, QExpr)]
maybeChildExprs path (Just e) = childExprs (path._Just) e
maybeChildExprs path Nothing = []

paramsChildExprs :: Traversal' QExpr [QParameter] -> [QParameter] -> [(QExprPath, QExpr)]
paramsChildExprs path ps = concat [paramChildExprs (path.ix i) p | (p, i) <- zip ps [0..]]

paramChildExprs :: Traversal' QExpr QParameter -> QParameter -> [(QExprPath, QExpr)]
paramChildExprs path (Param _ annot def _) =
    maybeChildExprs (path.param_py_annotationL) annot ++
        maybeChildExprs (path.param_defaultL) def
paramChildExprs path (VarArgsPos _ annot _) =
    maybeChildExprs (path.param_py_annotationL) annot
paramChildExprs path (VarArgsKeyword _ annot _) =
    maybeChildExprs (path.param_py_annotationL) annot
paramChildExprs path (EndPositional _) = []
paramChildExprs path (UnPackTuple _ def _) =
    maybeChildExprs (path.param_defaultL) def

maybeYieldArg :: Traversal' QExpr (Maybe QYieldArg) -> Maybe QYieldArg -> [(QExprPath, QExpr)]
maybeYieldArg path (Just ya) = yieldArg (path._Just) ya
maybeYieldArg path Nothing = []

yieldArg :: Traversal' QExpr QYieldArg -> QYieldArg -> [(QExprPath, QExpr)]
yieldArg path (YieldFrom e _) = childExprs (path._YieldFrom._1) e
yieldArg path (YieldExpr e) = childExprs (path._YieldExpr) e
