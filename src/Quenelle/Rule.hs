{-# LANGUAGE ImpredicativeTypes #-}
module Quenelle.Rule (
    ExprPred,
    ExprRule(..),
    parseExprRule,
    runExprRule
) where

import Control.Applicative
import Control.Exception.Base
import Control.Lens
import Control.Monad.State.Strict

import Language.Python.Common.AST
import Language.Python.Common.ParseError
import Language.Python.Version2

import Quenelle.Lens
import Quenelle.Normalize
import Quenelle.Var

type ArgumentPred = QArgument -> Bool
type ExprPred = QExpr -> Bool
type ParameterPred = QParameter -> Bool

data ExprRule = ExprRule {
    exprRuleName :: String,
    exprRulePred :: ExprPred,
    exprRuleBoundVars :: [(String, QPath)]
    }


runExprRule :: ExprRule -> QExpr -> Maybe [(String, QExpr)]
runExprRule rule expr =
    case (exprRulePred rule) expr of
        False -> Nothing
        True -> readExprVars expr (exprRuleBoundVars rule)

readExprVars :: QExpr -> [(String, QPath)] -> Maybe [(String, QExpr)]
readExprVars expr vars = mapM (readExprPath expr) vars

readExprPath :: QExpr -> (String, QPath) -> Maybe (String, QExpr)
-- If exprRulePred succeeds, then every path should be valid
readExprPath expr (name, path) = assert (has path expr) $ expr ^? path >>= \e -> return (name, e)


parseExprRule :: String -> Either ParseError ExprRule
parseExprRule str =
    case parseExprPred str of
        Left err -> Left err
        Right (pred, state) -> Right ExprRule {
            exprRuleName = "rule",
            exprRulePred = pred,
            exprRuleBoundVars = ruleBoundVars state
            }


parseExprPred :: String -> Either ParseError (ExprPred, RuleState)
parseExprPred str =
    case parseExpr str "rule" of
        Left err -> Left err
        Right (e, _) -> Right $ runState (exprToPred id $ normalizeExpr e) emptyRuleState

--------------------------------------------------------------------------------

data RuleState = RuleState {
    ruleBoundVars :: [(String, QPath)]
    }

emptyRuleState = RuleState { ruleBoundVars = [] }

bindVar :: String -> QPath -> State RuleState ()
bindVar name path = modify $ \s -> s { ruleBoundVars = ruleBoundVars s ++ [(name, path)] }

--------------------------------------------------------------------------------

exprToPred :: QPath -> QExpr -> State RuleState ExprPred
exprToPred path (Var x _) =
    case classifyVar x of
        (Expression, y) -> return pExpr'
        (BoundExpression, y) -> do
            bindVar y path
            return pExpr'
        (Variable, y) -> return pVar'
        (Normal, _) -> return $ pVar (pIdent x)

exprToPred path (Int x _ _) = return $ pInt x
exprToPred path (LongInt x _ _) = return $ pLongInt x
exprToPred path (Bool x _) = return $ pBool x

exprToPred path (BinaryOp op l r _) = do
    lp <- exprToPred (path.left_op_argL) l
    rp <- exprToPred (path.right_op_argL) r
    return $ pBinaryOp (pOp op) lp rp

exprToPred path (UnaryOp op arg _) = do
    argp <- exprToPred (path.op_argL) arg
    return $ pUnaryOp (pOp op) argp

exprToPred path (Call fun [] _) = do
    funp <- exprToPred (path.call_funL) fun
    return $ pCall funp

exprToPred path (Paren e _) = pParen <$> exprToPred (path.paren_exprL) e

--------------------------------------------------------------------------------

pExpr' :: ExprPred
pExpr' _ = True

pIdent x (Ident y _) = ident_string x == y

pVar' :: ExprPred
pVar' Var{} = True
pVar' _ = False

pVar :: (QIdent -> Bool) -> ExprPred
pVar namep (Var name _) = namep name
pVar _ _ = False


pInt :: Integer -> ExprPred
pInt x (Int y _ _) = x == y
pInt _ _ = False


pLongInt :: Integer -> ExprPred
pLongInt x (Int y _ _) = x == y
pLongInt _ _ = False


pBool :: Bool -> ExprPred
pBool x (Bool y _) = x == y
pBool _ _  = False


pOp :: QOp -> QOp -> Bool
pOp x y = x == y

pBinaryOp :: (QOp -> Bool) -> ExprPred -> ExprPred -> ExprPred
pBinaryOp opp lp rp (BinaryOp op l r _) = and [opp op, lp l, rp r]
pBinaryOp _ _ _ _ = False

pUnaryOp :: (QOp -> Bool) -> ExprPred -> ExprPred
pUnaryOp opp argp (UnaryOp op arg _) = opp op && argp arg
pUnaryOp _ _ _ = False


pCall :: ExprPred -> ExprPred
pCall funp (Call fun [] _) = funp fun
pCall _ _ = False

pParen :: ExprPred -> ExprPred
pParen ep (Paren e _) = ep e
pParen _ _ = False
