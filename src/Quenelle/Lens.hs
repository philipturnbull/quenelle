{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Quenelle.Lens where

import Control.Lens

import Language.Python.Common.AST

type QArgument = Argument ()
type QCompFor = CompFor ()
type QCompIf = CompIf ()
type QCompIter = CompIter ()
type QComprehension a = Comprehension a ()
type QExpr = Expr ()
type QIdent = Ident ()
type QOp = Op ()
type QParameter = Parameter ()
type QParamTuple = ParamTuple ()
type QSlice = Slice ()

type QPath = Traversal QExpr QExpr QExpr QExpr

-- Ident
---- Ident
makeLensesFor [
    ("ident_string", "ident_stringL")
    ] ''Ident
---- annot
makeLensesFor [
    ("ident_annot", "ident_annotL")
    ] ''Ident

-- Expr
---- Var
makeLensesFor [
    ("var_ident", "var_identL")
    ] ''Expr
---- Int
makeLensesFor [
    ("int_value", "int_valueL"),
    ("expr_literal", "expr_literalL")
    ] ''Expr
---- LongInt
makeLensesFor [
--  ("int_value", "int_valueL"),
--  ("expr_literal", "expr_literalL")
    ] ''Expr
---- Float
makeLensesFor [
    ("float_value", "float_valueL")
--  ("expr_literal", "expr_literalL")
    ] ''Expr
---- Imaginary
makeLensesFor [
    ("imaginary_value", "imaginary_valueL")
--  ("expr_literal", "expr_literalL")
    ] ''Expr
---- Bool
makeLensesFor [
    ("bool_value", "bool_valueL")
    ] ''Expr
---- ByteStrings
makeLensesFor [
    ("byte_string_strings", "byte_string_stringsL")
    ] ''Expr
---- Strings
makeLensesFor [
    ("strings_strings", "strings_stringsL")
    ] ''Expr
---- UnicodeStrings
makeLensesFor [
    ("unicodestrings_strings", "unicodestrings_stringsL")
    ] ''Expr
---- Call
makeLensesFor [
    ("call_fun", "call_funL"),
    ("call_args", "call_argsL")
    ] ''Expr
---- Subscript
makeLensesFor [
    ("subscriptee", "subscripteeL"),
    ("subscript_expr", "subscript_exprL")
    ] ''Expr
---- SlicedExpr
makeLensesFor [
    ("slicee", "sliceeL"),
    ("slices", "slicesL")
    ] ''Expr
---- CondExpr
makeLensesFor [
    ("ce_true_branch", "ce_true_branchL"),
    ("ce_condition", "ce_conditionL"),
    ("ce_false_branch", "ce_false_branchL")
    ] ''Expr
---- BinaryOp
makeLensesFor [
    ("operator", "operatorL"),
    ("left_op_arg", "left_op_argL"),
    ("right_op_arg", "right_op_argL")
    ] ''Expr
---- UnaryOp
makeLensesFor [
--  ("operator", "operatorL")
    ("op_arg", "op_argL")
    ] ''Expr
---- Lambda
makeLensesFor [
    ("lambda_args", "lambda_argsL"),
    ("lambda_body", "lambda_bodyL")
    ] ''Expr
---- Tuple
makeLensesFor [
    ("tuple_exprs", "tuple_exprsL")
    ] ''Expr
---- Yield
makeLensesFor [
    ("yield_expr", "yield_exprL")
    ] ''Expr
---- Generator
makeLensesFor [
    ("gen_comprehension", "gen_comprehensionL")
    ] ''Expr
---- ListComp
makeLensesFor [
    ("list_comprehension", "list_comprehensionL")
    ] ''Expr
---- List
makeLensesFor [
    ("list_exprs", "list_exprsL")
    ] ''Expr
---- Dictionary
makeLensesFor [
    ("dict_mappings", "dict_mappingsL")
    ] ''Expr
---- DictComp
makeLensesFor [
    ("dict_comprehension", "dict_comprehensionL")
    ] ''Expr
---- Set
makeLensesFor [
    ("set_exprs", "set_exprsL")
    ] ''Expr
---- SetComp
makeLensesFor [
    ("set_comprehension", "set_comprehensionL")
    ] ''Expr
---- Starred
makeLensesFor [
    ("starred_expr", "starred_exprL")
    ] ''Expr
---- Paren
makeLensesFor [
    ("paren_expr", "paren_exprL")
    ] ''Expr
---- StringConversion
makeLensesFor [
    ("backquoted_exp", "backquoted_expL")
    ] ''Expr
---- annot
makeLensesFor [
    ("expr_annot", "expr_annotL")
    ] ''Expr

-- Argument
---- ArgExpr
makeLensesFor [
    ("arg_expr", "arg_exprL")
    ] ''Argument
---- ArgVarArgsPos
makeLensesFor [
--  ("arg_expr", "arg_exprL")
    ] ''Argument
---- ArgVarArgsKeyword
makeLensesFor [
--  ("arg_expr", "arg_exprL")
    ] ''Argument
---- ArgKeyword
makeLensesFor [
    ("arg_keyword", "arg_keywordL")
--  ("arg_expr", "arg_exprL")
    ] ''Argument
---- annot
makeLensesFor [
    ("arg_annot", "arg_annotL")
    ] ''Argument

-- Parameter
---- Param
makeLensesFor [
    ("param_name", "param_nameL"),
    ("param_py_annotation", "param_py_annotationL"),
    ("param_default", "param_defaultL")
    ] ''Parameter
---- VarArgsPos
makeLensesFor [
--  ("param_name", "param_nameL"),
--  ("param_py_annotation", "param_py_annotationL")
    ] ''Parameter
---- VarArgsKeyword
makeLensesFor [
--  ("param_name", "param_nameL"),
--  ("param_py_annotation", "param_py_annotationL")
    ] ''Parameter
---- UnPackTUple
makeLensesFor [
    ("param_unpack_tuple", "param_unpack_tupleL")
--  ("param_default", "param_defaultL")
    ] ''Parameter
---- annot
makeLensesFor [
    ("param_annot", "param_annotL")
    ] ''Parameter

-- ParamTuple
---- ParamTupleName
makeLensesFor [
    ("param_tuple_name", "param_tuple_nameL")
    ] ''ParamTuple
---- ParamTuple
makeLensesFor [
    ("param_tuple", "param_tupleL")
    ] ''ParamTuple
---- annot
makeLensesFor [
    ("param_tuple_annot", "param_tuple_annotL")
    ] ''ParamTuple

-- Comprehension
---- Comprehension
makeLensesFor [
    ("comprehension_expr", "comprehension_exprL"),
    ("comprehension_for", "comprehension_forL")
    ] ''Comprehension
---- annot
makeLensesFor [
    ("comprehension_annot", "comprehension_annotL")
    ] ''Comprehension

-- CompFor
---- CompFor
makeLensesFor [
    ("comp_for_exprs", "comp_for_exprsL"),
    ("comp_in_expr", "comp_in_exprL"),
    ("comp_for_iter", "comp_for_iterL")
    ] ''CompFor
---- annot
makeLensesFor [
    ("comp_for_annot", "comp_for_annotL")
    ] ''CompFor

-- CompIf
---- CompIf
makeLensesFor [
    ("comp_if", "comp_ifL"),
    ("comp_if_iter", "comp_if_iterL")
    ] ''CompIf
---- annot
makeLensesFor [
    ("comp_if_annot", "comp_if_annotL")
    ] ''CompIf

-- CompIter
---- IterFor
makeLensesFor [
    ("comp_iter_for", "comp_iter_forL")
    ] ''CompIter
---- IterIf
makeLensesFor [
    ("comp_iter_if", "comp_iter_ifL")
    ] ''CompIter
---- annot
makeLensesFor [
    ("comp_iter_annot", "comp_iter_annotL")
    ] ''CompIter

-- Slice
---- SliceProper
makeLensesFor [
    ("slice_lower", "slice_lowerL"),
    ("slice_upper", "slice_upperL"),
    ("slice_stride", "slice_strideL")
    ] ''Slice
---- SliceExpr
makeLensesFor [
    ("slice_expr", "slice_exprL")
    ] ''Slice
---- annot
makeLensesFor [
    ("slice_annot", "slice_annotL")
    ] ''Slice

-- Op
---- annot
makeLensesFor [
    ("op_annot", "op_annotL")
    ] ''Op

