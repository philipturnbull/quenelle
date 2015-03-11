{-# LANGUAGE ImpredicativeTypes #-}
module Quenelle.Var (
    VarType(..),
    classifyVar,
    validateVars
) where

import Data.Char
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty
import Language.Python.Common.AST

import Quenelle.Lens

data VarType = Expression | BoundExpression | Variable | BoundVariable | Normal
    deriving(Eq, Show)

classifyVar :: QIdent -> (VarType, String)
classifyVar x = f (ident_string x)
    where f y@"E" = (Expression, y)
          f y@('E':num) | all isDigit num = (BoundExpression, y)
          f y@"V" = (Variable, y)
          f y@('V':num) | all isDigit num = (BoundVariable, y)
          f y = (Normal, y)

validateVars :: (Eq a) => [(String, a)] -> Maybe [(String, a)]
validateVars vars = mapM validateVar $ Map.toList $ groupVars vars

validateVar :: (Eq a) => (String, NonEmpty a) -> Maybe (String, a)
validateVar (name, NonEmpty e []) = Just (name, e)
validateVar (name, NonEmpty e es) =
    if all (== e) es then -- only bind multiple variables if they all match
        Just (name, e) else Nothing

groupVars :: (Eq a) => [(String, a)] -> Map.Map String (NonEmpty a)
groupVars = f Map.empty
    where f acc [] = acc
          f acc ((k, v):vars) = f (Map.alter (upsert v) k acc) vars
          upsert v (Just (NonEmpty h t)) = Just $ NonEmpty h (v : t)
          upsert v Nothing = Just $ NonEmpty v []


