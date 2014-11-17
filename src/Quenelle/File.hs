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
        Right (m, _) -> Right $ moduleExprs m

moduleExprsFromFile :: FilePath -> IO (Either String [(ExprLocation, QExpr)])
moduleExprsFromFile filename = moduleExprsFromString filename <$> readFile filename

moduleExprs :: ModuleSpan -> [(ExprLocation, QExpr)]
moduleExprs (Module stmts) = mapMaybe topLevelExpr stmts
    where topLevelExpr (StmtExpr e span) = Just (exprLocation span, normalizeExpr e)
          topLevelExpr _ = Nothing

exprLocation (SpanCoLinear filename row _ _) = ExprLocation filename row
exprLocation (SpanMultiLine filename row _ _ _) = ExprLocation filename row
exprLocation (SpanPoint filename row _) = ExprLocation filename row
exprLocation _ = ExprLocation "unknown" 0
