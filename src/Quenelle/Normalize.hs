module Quenelle.Normalize (
    normalizeExpr
) where

import Language.Python.Common.AST

import Control.Monad
import Quenelle.Lens

-- | Convert a @ExprSpan@ (@Expr@ @SrcSpan@) into a @Expr@ @()@. In many places
--   we want to check if two AST subtrees are equivalent. This fails when using
--   @ExprSpan@ because the @SrcSpan@s are not equal. The implementation has been
--   mostly auto-generated.
normalizeExpr :: Expr a -> QExpr
normalizeExpr = void
