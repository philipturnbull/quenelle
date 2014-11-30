module Quenelle.File (
    TopLevelExpr(..),
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

data TopLevelExpr = TopLevelExpr StatementSpan ExprLocation QExpr

moduleExprsFromString :: FilePath -> String -> Either String [TopLevelExpr]
moduleExprsFromString filename contents =
    case parseModule contents filename of
        Left err -> Left $ show err
        Right (m, _) -> Right $ allModuleExprs m

moduleExprsFromFile :: FilePath -> IO (Either String [TopLevelExpr])
moduleExprsFromFile filename = moduleExprsFromString filename <$> readFile filename

allModuleExprs :: ModuleSpan -> [TopLevelExpr]
allModuleExprs (Module stmts) = concatMap stmtExprs stmts

stmtExprs :: StatementSpan -> [TopLevelExpr]
stmtExprs s@(While cond body else_ _) = [normalizeAndSpan s cond] ++ suiteExprs body ++ suiteExprs else_
stmtExprs s@(For tgts gen body else_ _) = map (normalizeAndSpan s) (tgts ++ [gen]) ++ suiteExprs body ++ suiteExprs else_
stmtExprs s@(Assign tos expr _) = map (normalizeAndSpan s) (tos ++ [expr])
stmtExprs s@(AugmentedAssign lhs _ rhs _) = map (normalizeAndSpan s) [lhs, rhs]
stmtExprs s@(Return (Just expr) _) = [normalizeAndSpan s expr]
stmtExprs s@(Return Nothing _) = []
stmtExprs s@(With ctxt body _) = map (normalizeAndSpan s) (concatMap flattenWithContext ctxt) ++ suiteExprs body
stmtExprs s@(Delete exprs _) = map (normalizeAndSpan s) exprs
stmtExprs s@(StmtExpr e _) = [normalizeAndSpan s e]
stmtExprs s@(Assert exprs _) = map (normalizeAndSpan s) exprs
stmtExprs s@(Print _ exprs _ _) = map (normalizeAndSpan s) exprs
stmtExprs s@(Exec expr scope _) = map (normalizeAndSpan s) $ [expr] ++ flattenExecScope scope
stmtExprs _ = []

flattenExecScope :: Maybe (ExprSpan, Maybe ExprSpan) -> [ExprSpan]
flattenExecScope (Just (l, (Just r))) = [l, r]
flattenExecScope (Just (l, Nothing)) = [l]
flattenExecScope Nothing = []

flattenWithContext :: (ExprSpan, Maybe ExprSpan) -> [ExprSpan]
flattenWithContext (e1, Just e2) = [e1, e2]
flattenWithContext (e1, Nothing) = [e1]

suiteExprs :: SuiteSpan -> [TopLevelExpr]
suiteExprs = concatMap stmtExprs

normalizeAndSpan :: StatementSpan -> ExprSpan -> TopLevelExpr
normalizeAndSpan s e = TopLevelExpr s (exprLocation (expr_annot e)) (normalizeExpr e)

exprLocation (SpanCoLinear filename row _ _) = ExprLocation filename row
exprLocation (SpanMultiLine filename row _ _ _) = ExprLocation filename row
exprLocation (SpanPoint filename row _) = ExprLocation filename row
exprLocation _ = ExprLocation "unknown" 0
