module Quenelle.File (
    ExprLocation(..),

    moduleExprsFromFile,
    moduleExprsFromString
) where

import Control.Applicative
import Data.Maybe

import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Language.Python.Version2.Parser

import Quenelle.Lens
import Quenelle.Normalize

data ExprLocation = ExprLocation !String !Int

instance Show ExprLocation where
    show (ExprLocation filename line) = filename ++ ":" ++ show line

moduleExprsFromString :: FilePath -> String -> Either String [(ExprLocation, QExpr)]
moduleExprsFromString filename contents =
    case parseModule contents filename of
        Left err -> Left $ show err
        Right (m, _) -> Right $ allModuleExprs m

moduleExprsFromFile :: FilePath -> IO (Either String [(ExprLocation, QExpr)])
moduleExprsFromFile filename = moduleExprsFromString filename <$> readFile filename

allModuleExprs :: ModuleSpan -> [(ExprLocation, QExpr)]
allModuleExprs (Module stmts) = concatMap stmtExprs stmts

stmtExprs :: StatementSpan -> [(ExprLocation, QExpr)]
stmtExprs (While cond body else_ _) = [normalizeAndSpan cond] ++ suiteExprs body ++ suiteExprs else_
stmtExprs (For tgts gen body else_ _) = map normalizeAndSpan (tgts ++ [gen]) ++ suiteExprs body ++ suiteExprs else_
stmtExprs (Assign tos expr _) = map normalizeAndSpan (tos ++ [expr])
stmtExprs (AugmentedAssign lhs _ rhs _) = map normalizeAndSpan [lhs, rhs]
stmtExprs (Return (Just expr) _) = [normalizeAndSpan expr]
stmtExprs (Return Nothing _) = []
stmtExprs (With ctxt body _) = map normalizeAndSpan (concatMap flattenWithContext ctxt) ++ suiteExprs body
stmtExprs (Delete exprs _) = map normalizeAndSpan exprs
stmtExprs (StmtExpr e _) = [normalizeAndSpan e]
stmtExprs (Assert exprs _) = map normalizeAndSpan exprs
stmtExprs (Print _ exprs _ _) = map normalizeAndSpan exprs
stmtExprs (Exec expr scope _) = map normalizeAndSpan $ [expr] ++ flattenExecScope scope
stmtExprs _ = []

flattenExecScope :: Maybe (ExprSpan, Maybe ExprSpan) -> [ExprSpan]
flattenExecScope (Just (l, (Just r))) = [l, r]
flattenExecScope (Just (l, Nothing)) = [l]
flattenExecScope Nothing = []

flattenWithContext :: (ExprSpan, Maybe ExprSpan) -> [ExprSpan]
flattenWithContext (e1, Just e2) = [e1, e2]
flattenWithContext (e1, Nothing) = [e1]

suiteExprs :: SuiteSpan -> [(ExprLocation, QExpr)]
suiteExprs = concatMap stmtExprs

normalizeAndSpan e = (exprLocation (expr_annot e), normalizeExpr e)

exprLocation (SpanCoLinear filename row _ _) = ExprLocation filename row
exprLocation (SpanMultiLine filename row _ _ _) = ExprLocation filename row
exprLocation (SpanPoint filename row _) = ExprLocation filename row
exprLocation _ = ExprLocation "unknown" 0
