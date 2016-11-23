{-# LANGUAGE ImpredicativeTypes #-}
module Quenelle.Var (
    ExpressionID(..),
    VariableID(..),
    VarType(..),
    Binding(..),
    classifyVar,
    validateBindings
) where

import Data.Char
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty
import Language.Python.Common.AST

import Quenelle.Lens

data ExpressionID = ExpressionID Int deriving(Eq,Ord)

instance Show ExpressionID where
  show (ExpressionID x) = "E" ++ show x

data VariableID = VariableID Int deriving(Eq,Ord)

instance Show VariableID where
  show (VariableID x) = "V" ++ show x

data Binding =
      VariableBinding VariableID QIdent
    | ExpressionBinding ExpressionID QExpr
    deriving(Eq, Ord, Show)

data VarType =
      Expression
    | BoundExpression ExpressionID
    | Variable
    | BoundVariable VariableID
    | Normal
    deriving(Eq, Show)

classifyVar :: QIdent -> VarType
classifyVar x = f (ident_string x)
    where f "E" = Expression
          f ('E':num) | all isDigit num = BoundExpression $ ExpressionID $ read num
          f "V" = Variable
          f ('V':num) | all isDigit num = BoundVariable $ VariableID $ read num
          f _ = Normal

validateBindings :: [Binding] -> Maybe [Binding]
validateBindings bs = do
    exprs <- mapM validateExprs $ Map.toList groupedExprs
    vars <- mapM validateVars $ Map.toList groupedVars
    return $ exprs ++ vars
    where (groupedExprs, groupedVars) = groupBindings bs

validateExprs :: (ExpressionID, NonEmpty QExpr) -> Maybe Binding
validateExprs (name, NonEmpty e []) = Just (ExpressionBinding name e)
validateExprs (name, NonEmpty e es) =
    if all (== e) es then -- only bind multiple expressions if they all match
        Just (ExpressionBinding name e) else Nothing

validateVars :: (VariableID, NonEmpty QIdent) -> Maybe Binding
validateVars (name, NonEmpty e []) = Just (VariableBinding name e)
validateVars (name, NonEmpty e es) =
    if all (== e) es then -- only bind multiple expressions if they all match
        Just (VariableBinding name e) else Nothing

type BindingMap k v = Map.Map k (NonEmpty v)

groupBindings :: [Binding] -> (BindingMap ExpressionID QExpr, BindingMap VariableID QIdent)
groupBindings = f Map.empty Map.empty
    where f eacc vacc [] = (eacc, vacc)
          f eacc vacc (VariableBinding k v:bs) = f eacc (Map.alter (upsert v) k vacc) bs
          f eacc vacc (ExpressionBinding k v:bs) = f (Map.alter (upsert v) k eacc) vacc bs
          upsert v (Just (NonEmpty h t)) = Just $ NonEmpty h (v : t)
          upsert v Nothing = Just $ NonEmpty v []
