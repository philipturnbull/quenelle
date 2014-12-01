{-# LANGUAGE RecordWildCards #-}
module Quenelle.Normalize (
    normalizeExpr
) where

import Language.Python.Common.AST

import Quenelle.Lens

-- | Convert a @ExprSpan@ (@Expr@ @SrcSpan@) into a @Expr@ @()@. In many places
--   we want to check if two AST subtrees are equivalent. This fails when using
--   @ExprSpan@ because the @SrcSpan@s are not equal. The implementation has been
--   mostly auto-generated.
normalizeExpr :: Expr a -> QExpr
normalizeExpr e = fmap (\_ -> ()) e


instance Functor Ident where
    fmap f Ident{..} = Ident ident_string (f ident_annot)


instance Functor Op where
    fmap f op = op { op_annot = f (op_annot op) }


-- We can't define a Functor instance for Comprehension because of the first type paramater.
fmapExprComprehension :: (a -> b) -> Comprehension (Expr a) a -> Comprehension (Expr b) b
fmapExprComprehension f Comprehension{..} = Comprehension (fmap f comprehension_expr) (fmap f comprehension_for) (f comprehension_annot)

fmapExprExprComprehension :: (a -> b) -> Comprehension (Expr a, Expr a) a -> Comprehension (Expr b, Expr b) b
fmapExprExprComprehension f Comprehension{..} = Comprehension (fmap f l, fmap f r) (fmap f comprehension_for) (f comprehension_annot)
    where (l, r) = comprehension_expr


instance Functor Expr where
    fmap f Var{..} = Var (fmap f var_ident) (f expr_annot)
    fmap f Int{..} = Int int_value expr_literal (f expr_annot)
    fmap f LongInt{..} = LongInt int_value expr_literal (f expr_annot)
    fmap f Float{..} = Float float_value expr_literal (f expr_annot)
    fmap f Imaginary{..} = Imaginary imaginary_value expr_literal (f expr_annot)
    fmap f Bool{..} = Bool bool_value (f expr_annot)
    fmap f None{..} = None (f expr_annot)
    fmap f Ellipsis{..} = Ellipsis (f expr_annot)
    fmap f ByteStrings{..} = ByteStrings byte_string_strings (f expr_annot)
    fmap f Strings{..} = Strings strings_strings (f expr_annot)
    fmap f UnicodeStrings{..} = UnicodeStrings unicodestrings_strings (f expr_annot)
    fmap f Call{..} = Call (fmap f call_fun) (fmap (fmap f) call_args) (f expr_annot)
    fmap f Subscript{..} = Subscript (fmap f subscriptee) (fmap f subscript_expr) (f expr_annot)
    fmap f SlicedExpr{..} = SlicedExpr (fmap f slicee) (fmap (fmap f) slices) (f expr_annot)
    fmap f CondExpr{..} = CondExpr (fmap f ce_true_branch) (fmap f ce_condition) (fmap f ce_false_branch) (f expr_annot)
    fmap f BinaryOp{..} = BinaryOp (fmap f operator) (fmap f left_op_arg) (fmap f right_op_arg) (f expr_annot)
    fmap f UnaryOp{..} = UnaryOp (fmap f operator) (fmap f op_arg) (f expr_annot)
    fmap f Lambda{..} = Lambda (fmap (fmap f) lambda_args) (fmap f lambda_body) (f expr_annot)
    fmap f Tuple{..} = Tuple (fmap (fmap f) tuple_exprs) (f expr_annot)
    fmap f Yield{..} = Yield (fmap (fmap f) yield_expr) (f expr_annot)
    fmap f Generator{..} = Generator (fmapExprComprehension f gen_comprehension) (f expr_annot)
    fmap f ListComp{..} = ListComp (fmapExprComprehension f list_comprehension) (f expr_annot)
    fmap f List{..} = List (fmap (fmap f) list_exprs) (f expr_annot)
    fmap f Dictionary{..} = Dictionary (fmap (\(l, r) -> (fmap f l, fmap f r)) dict_mappings) (f expr_annot)
    fmap f DictComp{..} = DictComp (fmapExprExprComprehension f dict_comprehension) (f expr_annot)
    fmap f Set{..} = Set (fmap (fmap f) set_exprs) (f expr_annot)
    fmap f SetComp{..} = SetComp (fmapExprComprehension f set_comprehension) (f expr_annot)
    fmap f Starred{..} = Starred (fmap f starred_expr) (f expr_annot)
    fmap f Paren{..} = Paren (fmap f paren_expr) (f expr_annot)
    -- Gah! expr_anot is mispelled for StringConversion
    fmap f StringConversion{..} = StringConversion (fmap f backquoted_expr) (f expr_anot)


instance Functor Argument where
    fmap f ArgExpr{..} = ArgExpr (fmap f arg_expr) (f arg_annot)
    fmap f ArgVarArgsPos{..} = ArgVarArgsPos (fmap f arg_expr) (f arg_annot)
    fmap f ArgVarArgsKeyword{..} = ArgVarArgsKeyword (fmap f arg_expr) (f arg_annot)
    fmap f ArgKeyword{..} = ArgKeyword (fmap f arg_keyword) (fmap f arg_expr) (f arg_annot)


instance Functor Parameter where
    fmap f Param{..} = Param (fmap f param_name) (fmap (fmap f) param_py_annotation) (fmap (fmap f) param_default) (f param_annot)
    fmap f VarArgsPos{..} = VarArgsPos (fmap f param_name) (fmap (fmap f) param_py_annotation) (f param_annot)
    fmap f VarArgsKeyword{..} = VarArgsKeyword (fmap f param_name) (fmap (fmap f) param_py_annotation) (f param_annot)
    fmap f EndPositional{..} = EndPositional (f param_annot)
    fmap f UnPackTuple{..} = UnPackTuple (fmap f param_unpack_tuple) (fmap (fmap f) param_default) (f param_annot)


instance Functor ParamTuple where
    fmap f ParamTupleName{..} = ParamTupleName (fmap f param_tuple_name) (f param_tuple_annot)
    fmap f ParamTuple{..} = ParamTuple (fmap (fmap f) param_tuple) (f param_tuple_annot)


instance Functor Slice where
    fmap f SliceProper{..} = SliceProper (fmap (fmap f) slice_lower) (fmap (fmap f) slice_upper) (fmap (fmap (fmap f)) slice_stride) (f slice_annot)
    fmap f SliceExpr{..} = SliceExpr (fmap f slice_expr) (f slice_annot)
    fmap f SliceEllipsis{..} = SliceEllipsis (f slice_annot)


instance Functor CompIf where
    fmap f CompIf{..} = CompIf (fmap f comp_if) (fmap (fmap f) comp_if_iter) (f comp_if_annot)


instance Functor CompFor where
    fmap f CompFor{..} = CompFor (fmap (fmap f) comp_for_exprs) (fmap f comp_in_expr) (fmap (fmap f) comp_for_iter) (f comp_for_annot)


instance Functor CompIter where
    fmap f IterFor{..} = IterFor (fmap f comp_iter_for) (f comp_iter_annot)
    fmap f IterIf{..} = IterIf (fmap f comp_iter_if) (f comp_iter_annot)
