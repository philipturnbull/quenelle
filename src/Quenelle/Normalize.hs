module Quenelle.Normalize (
    normalizeExpr
) where

import Control.Applicative
import Data.Functor.Identity

import Language.Python.Common.AST
import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST
import Language.Python.Version2

import Quenelle.Lens

-- | Convert a @ExprSpan@ (@Expr@ @SrcSpan@) into a @Expr@ @()@. In many places
--   we want to check if two AST subtrees are equivalent. This fails when using
--   @ExprSpan@ because the @SrcSpan@s are not equal. The implementation has been
--   mostly auto-generated.
normalizeExpr :: Expr a -> QExpr
normalizeExpr e = runIdentity $ _normalizeExpr e


_normalizeIdent (Ident ident_string ident_annot) = return $ Ident ident_string ()


_normalizeExpr (Var var_ident expr_annot) = do
    var_ident' <- _normalizeIdent var_ident
    return $ Var var_ident' ()
_normalizeExpr (Int int_value expr_literal expr_annot) = return $ Int int_value expr_literal ()
_normalizeExpr (LongInt int_value expr_literal expr_annot) = return $ LongInt int_value expr_literal ()
_normalizeExpr (Float float_value expr_literal expr_annot) = return $ Float float_value expr_literal ()
_normalizeExpr (Imaginary imaginary_value expr_literal expr_annot) = return $ Imaginary imaginary_value expr_literal ()
_normalizeExpr (Bool bool_value expr_annot) = return $ Bool bool_value ()
_normalizeExpr (None expr_annot) = return $ None ()
_normalizeExpr (Ellipsis expr_annot) = return $ Ellipsis ()
_normalizeExpr (ByteStrings byte_string_strings expr_annot) = return $ ByteStrings byte_string_strings ()
_normalizeExpr (Strings strings_strings expr_annot) = return $ Strings strings_strings ()
_normalizeExpr (UnicodeStrings unicodestrings_strings expr_annot) = return $ UnicodeStrings unicodestrings_strings ()
_normalizeExpr (Call call_fun call_args expr_annot) = do
    call_fun' <- _normalizeExpr call_fun
    call_args' <- mapM _normalizeArgument call_args
    return $ Call call_fun' call_args' ()
_normalizeExpr (Subscript subscriptee subscript_expr expr_annot) = do
    subscriptee' <- _normalizeExpr subscriptee
    subscript_expr' <- _normalizeExpr subscript_expr
    return $ Subscript subscriptee' subscript_expr' ()
_normalizeExpr (SlicedExpr slicee slices expr_annot) = do
    slicee' <- _normalizeExpr slicee
    slices' <- mapM _normalizeSlice slices
    return $ SlicedExpr slicee' slices' ()
_normalizeExpr (CondExpr ce_true_branch ce_condition ce_false_branch expr_annot) = do
    ce_true_branch' <- _normalizeExpr ce_true_branch
    ce_condition' <- _normalizeExpr ce_condition
    ce_false_branch' <- _normalizeExpr ce_false_branch
    return $ CondExpr ce_true_branch' ce_condition' ce_false_branch' ()
_normalizeExpr (BinaryOp operator left_op_arg right_op_arg expr_annot) = do
    operator' <- _normalizeOp operator
    left_op_arg' <- _normalizeExpr left_op_arg
    right_op_arg' <- _normalizeExpr right_op_arg
    return $ BinaryOp operator' left_op_arg' right_op_arg' ()
_normalizeExpr (UnaryOp operator op_arg expr_annot) = do
    operator' <- _normalizeOp operator
    op_arg' <- _normalizeExpr op_arg
    return $ UnaryOp operator' op_arg' ()
_normalizeExpr (Lambda lambda_args lambda_body expr_annot) = do
    lambda_args' <- mapM _normalizeParameter lambda_args
    lambda_body' <- _normalizeExpr lambda_body
    return $ Lambda lambda_args' lambda_body' ()
_normalizeExpr (Tuple tuple_exprs expr_annot) = do
    tuple_exprs' <- mapM _normalizeExpr tuple_exprs
    return $ Tuple tuple_exprs' ()
_normalizeExpr (Yield yield_expr expr_annot) = do
    yield_expr' <- maybe (return Nothing) (\x -> Just <$> _normalizeExpr x) yield_expr
    return $ Yield yield_expr' ()
_normalizeExpr (Generator gen_comprehension expr_annot) = do
    gen_comprehension_expr' <- _normalizeExpr $ comprehension_expr gen_comprehension
    gen_comprehension_for' <- _normalizeCompFor $ comprehension_for gen_comprehension
    gen_comprehension' <- return $ Comprehension gen_comprehension_expr' gen_comprehension_for' ()
    return $ Generator gen_comprehension' ()
_normalizeExpr (ListComp list_comprehension expr_annot) = do
    list_comprehension_expr' <- _normalizeExpr $ comprehension_expr list_comprehension
    list_comprehension_for' <- _normalizeCompFor $ comprehension_for list_comprehension
    list_comprehension' <- return $ Comprehension list_comprehension_expr' list_comprehension_for' ()
    return $ ListComp list_comprehension' ()
_normalizeExpr (List list_exprs expr_annot) = do
    list_exprs' <- mapM _normalizeExpr list_exprs
    return $ List list_exprs' ()
_normalizeExpr (Dictionary dict_mappings expr_annot) = do
    dict_mappings' <- mapM (\(l, r) -> _normalizeExpr l >>= \l' -> _normalizeExpr r >>= \r' -> return (l', r')) dict_mappings
    return $ Dictionary dict_mappings' ()
_normalizeExpr (DictComp dict_comprehension expr_annot) = do
    dict_comprehension_l' <- _normalizeExpr $ fst $ comprehension_expr dict_comprehension
    dict_comprehension_r' <- _normalizeExpr $ snd $ comprehension_expr dict_comprehension
    dict_comprehension_for' <- _normalizeCompFor $ comprehension_for dict_comprehension
    dict_comprehension' <- return $ Comprehension (dict_comprehension_l', dict_comprehension_r') dict_comprehension_for' ()
    return $ DictComp dict_comprehension' ()
_normalizeExpr (Set set_exprs expr_annot) = do
    set_exprs' <- mapM _normalizeExpr set_exprs
    return $ Set set_exprs' ()
_normalizeExpr (SetComp set_comprehension expr_annot) = do
    set_comprehension_expr' <- _normalizeExpr $ comprehension_expr set_comprehension
    set_comprehension_for' <- _normalizeCompFor $ comprehension_for set_comprehension
    set_comprehension' <- return $ Comprehension set_comprehension_expr' set_comprehension_for' ()
    return $ SetComp set_comprehension' ()
_normalizeExpr (Starred starred_expr expr_annot) = do
    starred_expr' <- _normalizeExpr starred_expr
    return $ Starred starred_expr' ()
_normalizeExpr (Paren paren_expr expr_annot) = do
    paren_expr' <- _normalizeExpr paren_expr
    return $ Paren paren_expr' ()
_normalizeExpr (StringConversion backquoted_exp expr_annot) = do
    backquoted_exp' <- _normalizeExpr backquoted_exp
    return $ StringConversion backquoted_exp' ()


_normalizeArgument (ArgExpr arg_expr arg_annot) = do
    arg_expr' <- _normalizeExpr arg_expr
    return $ ArgExpr arg_expr' ()
_normalizeArgument (ArgVarArgsPos arg_expr arg_annot) = do
    arg_expr' <- _normalizeExpr arg_expr
    return $ ArgVarArgsPos arg_expr' ()
_normalizeArgument (ArgVarArgsKeyword arg_expr arg_annot) = do
    arg_expr' <- _normalizeExpr arg_expr
    return $ ArgVarArgsKeyword arg_expr' ()
_normalizeArgument (ArgKeyword arg_keyword arg_expr arg_annot) = do
    arg_keyword' <- _normalizeIdent arg_keyword
    arg_expr' <- _normalizeExpr arg_expr
    return $ ArgKeyword arg_keyword' arg_expr' ()


_normalizeParameter (Param param_name param_py_annotation param_default param_annot) = do
    param_name' <- _normalizeIdent param_name
    param_py_annotation' <- maybe (return Nothing) (\x -> Just <$> _normalizeExpr x) param_py_annotation
    param_default' <- maybe (return Nothing) (\x -> Just <$> _normalizeExpr x) param_default
    return $ Param param_name' param_py_annotation' param_default' ()
_normalizeParameter (VarArgsPos param_name param_py_annotation param_annot) = do
    param_name' <- _normalizeIdent param_name
    param_py_annotation' <- maybe (return Nothing) (\x -> Just <$> _normalizeExpr x) param_py_annotation
    return $ VarArgsPos param_name' param_py_annotation' ()
_normalizeParameter (VarArgsKeyword param_name param_py_annotation param_annot) = do
    param_name' <- _normalizeIdent param_name
    param_py_annotation' <- maybe (return Nothing) (\x -> Just <$> _normalizeExpr x) param_py_annotation
    return $ VarArgsKeyword param_name' param_py_annotation' ()
_normalizeParameter (EndPositional param_annot) = return $ EndPositional  ()
_normalizeParameter (UnPackTuple param_unpack_tuple param_default param_annot) = do
    param_unpack_tuple' <- _normalizeParamTuple param_unpack_tuple
    param_default' <- maybe (return Nothing) (\x -> Just <$> _normalizeExpr x) param_default
    return $ UnPackTuple param_unpack_tuple' param_default' ()


_normalizeParamTuple (ParamTupleName param_tuple_name param_tuple_annot) = do
    param_tuple_name' <- _normalizeIdent param_tuple_name
    return $ ParamTupleName param_tuple_name' ()
_normalizeParamTuple (ParamTuple param_tuple param_tuple_annot) = do
    param_tuple' <- mapM _normalizeParamTuple param_tuple
    return $ ParamTuple param_tuple' ()


_normalizeCompIter (IterFor comp_iter_for comp_iter_annot) = do
    comp_iter_for' <- _normalizeCompFor comp_iter_for
    return $ IterFor comp_iter_for' ()
_normalizeCompIter (IterIf comp_iter_if comp_iter_annot) = do
    comp_iter_if' <- _normalizeCompIf comp_iter_if
    return $ IterIf comp_iter_if' ()


_normalizeSlice (SliceProper slice_lower slice_upper slice_stride slice_annot) = do
    slice_lower' <- maybe (return Nothing) (\x -> Just <$> _normalizeExpr x) slice_lower
    slice_upper' <- maybe (return Nothing) (\x -> Just <$> _normalizeExpr x) slice_upper
    slice_stride' <- case slice_stride of
           Just (Just e) -> Just . Just <$> _normalizeExpr e
           _ -> return Nothing
    return $ SliceProper slice_lower' slice_upper' slice_stride' ()
_normalizeSlice (SliceExpr slice_expr slice_annot) = do
    slice_expr' <- _normalizeExpr slice_expr
    return $ SliceExpr slice_expr' ()
_normalizeSlice (SliceEllipsis slice_annot) = return $ SliceEllipsis ()


_normalizeOp (And op_annot) = return $ And  ()
_normalizeOp (Or op_annot) = return $ Or  ()
_normalizeOp (Not op_annot) = return $ Not  ()
_normalizeOp (Exponent op_annot) = return $ Exponent  ()
_normalizeOp (LessThan op_annot) = return $ LessThan  ()
_normalizeOp (GreaterThan op_annot) = return $ GreaterThan  ()
_normalizeOp (Equality op_annot) = return $ Equality  ()
_normalizeOp (GreaterThanEquals op_annot) = return $ GreaterThanEquals  ()
_normalizeOp (LessThanEquals op_annot) = return $ LessThanEquals  ()
_normalizeOp (NotEquals  op_annot) = return $ NotEquals   ()
_normalizeOp (NotEqualsV2  op_annot) = return $ NotEqualsV2   ()
_normalizeOp (In op_annot) = return $ In  ()
_normalizeOp (Is op_annot) = return $ Is  ()
_normalizeOp (IsNot op_annot) = return $ IsNot  ()
_normalizeOp (NotIn op_annot) = return $ NotIn  ()
_normalizeOp (BinaryOr op_annot) = return $ BinaryOr  ()
_normalizeOp (Xor op_annot) = return $ Xor  ()
_normalizeOp (BinaryAnd op_annot) = return $ BinaryAnd  ()
_normalizeOp (ShiftLeft op_annot) = return $ ShiftLeft  ()
_normalizeOp (ShiftRight op_annot) = return $ ShiftRight  ()
_normalizeOp (Multiply op_annot) = return $ Multiply  ()
_normalizeOp (Plus op_annot) = return $ Plus  ()
_normalizeOp (Minus op_annot) = return $ Minus  ()
_normalizeOp (Divide op_annot) = return $ Divide  ()
_normalizeOp (FloorDivide op_annot) = return $ FloorDivide  ()
_normalizeOp (Invert op_annot) = return $ Invert  ()
_normalizeOp (Modulo op_annot) = return $ Modulo  ()
_normalizeOp (Dot op_annot) = return $ Dot  ()


_normalizeCompFor (CompFor comp_for_exprs comp_in_expr comp_for_iter comp_for_annot) = do
    comp_for_exprs' <- mapM _normalizeExpr comp_for_exprs
    comp_in_expr' <- _normalizeExpr comp_in_expr
    comp_for_iter' <- maybe (return Nothing) (\x -> Just <$> _normalizeCompIter x) comp_for_iter
    return $ CompFor comp_for_exprs' comp_in_expr' comp_for_iter' ()


_normalizeCompIf (CompIf comp_if comp_if_iter comp_if_annot) = do
    comp_if' <- _normalizeExpr comp_if
    comp_if_iter' <- maybe (return Nothing) (\x -> Just <$> _normalizeCompIter x) comp_if_iter
    return $ CompIf comp_if' comp_if_iter' ()

