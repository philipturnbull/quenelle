{-# LANGUAGE ImpredicativeTypes #-}
module Quenelle.Rule (
    ExprPred,
    ExprRule(..),
    RuleBinding(..),
    parseExprRule,
    runExprRule
) where

import Control.Exception.Base
import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe

import Language.Python.Common.AST
import Language.Python.Common.ParseError
import Language.Python.Version2

import Quenelle.Lens
import Quenelle.Normalize
import Quenelle.Var

type ArgumentPred = QArgument -> Bool
type ArgumentsPred = [QArgument] -> Bool
type ExprPred = QExpr -> Bool
type ComprehensionPred = QComprehension -> Bool
type ComprehensionExprPred = QComprehensionExpr -> Bool
type CompIfPred = QCompIf -> Bool
type CompForPred = QCompFor -> Bool
type IdentPred = QIdent -> Bool
type ParameterPred = QParameter -> Bool
type ParametersPred = [QParameter] -> Bool

data RuleBinding =
      RuleVariableBinding VariableID QIdentPath
    | RuleExpressionBinding ExpressionID QExprPath

data ExprRule = ExprRule {
    exprRuleName :: String,
    exprRuleExpr :: QExpr,
    exprRulePred :: ExprPred,
    exprRuleBindings :: [RuleBinding]
    }


runExprRule :: ExprRule -> QExpr -> Maybe [Binding]
runExprRule rule expr =
    if exprRulePred rule expr then
        readExprExprs expr (exprRuleBindings rule) else Nothing

readExprExprs :: QExpr -> [RuleBinding] -> Maybe [Binding]
readExprExprs expr = mapM (readPath expr)
    -- If exprRulePred succeeds, then every path should be valid
    where readPath :: QExpr -> RuleBinding -> Maybe Binding
          readPath e (RuleVariableBinding name path) = assert (has path e) $ VariableBinding name <$> e ^? path
          readPath e (RuleExpressionBinding name path) = assert (has path e) $ ExpressionBinding name <$> e ^? path

parseExprRule :: String -> Either ParseError ExprRule
parseExprRule str =
    case parseExprPred str of
        Left err -> Left err
        Right (expr, pred, state) -> Right ExprRule {
            exprRuleName = "rule",
            exprRuleExpr = expr,
            exprRulePred = pred,
            exprRuleBindings = ruleBindings state
            }


parseExprPred :: String -> Either ParseError (QExpr, ExprPred, RuleState)
parseExprPred str =
    case parseExpr str "rule" of
        Left err -> Left err
        Right (e, _) -> Right $ runExprToPred e

runExprToPred e = (e', pred, state)
    where (pred, state) = runState (exprToPred id e') emptyRuleState
          e' = normalizeExpr e

--------------------------------------------------------------------------------

data RuleState = RuleState {
    ruleBindings :: [RuleBinding]
    }

emptyRuleState = RuleState { ruleBindings = [] }

bindVariable :: VariableID -> QIdentPath -> State RuleState ()
bindVariable name path = modify $ \s -> s {
    ruleBindings = ruleBindings s ++ [RuleVariableBinding name path]
    }

bindExpression :: ExpressionID -> QExprPath -> State RuleState ()
bindExpression name path = modify $ \s -> s {
    ruleBindings = ruleBindings s ++ [RuleExpressionBinding name path]
    }

--------------------------------------------------------------------------------

exprsToPred :: Traversal' QExpr [QExpr] -> [QExpr] -> State RuleState [ExprPred]
exprsToPred path es = sequence [exprToPred (path.ix i) e | (e, i) <- zip es [0..]]


exprToPred :: QExprPath -> QExpr -> State RuleState ExprPred
exprToPred path (Var x _) =
    case classifyVar x of
        Expression -> return pExpr'
        (BoundExpression name) -> do
            bindExpression name path
            return pExpr'
        Variable -> return pVar'
        (BoundVariable name) -> do
            bindVariable name (path.var_identL)
            return pVar'
        Normal -> return $ pVar (pIdent x)

exprToPred path (Int x _ _) = return $ pInt x
exprToPred path (LongInt x _ _) = return $ pLongInt x
exprToPred path (Float x _ _) = return $ pFloat x
exprToPred path (Imaginary x _ _) = return $ pImaginary x
exprToPred path (Bool x _) = return $ pBool x
exprToPred path (ByteStrings ss _) = return $ pByteStrings ss
exprToPred path (Strings ss _) = return $ pStrings ss
exprToPred path (UnicodeStrings ss _) = return $ pUnicodeStrings ss

exprToPred path (BinaryOp op l r _) = do
    lp <- exprToPred (path.left_op_argL) l
    rp <- exprToPred (path.right_op_argL) r
    return $ pBinaryOp (pOp op) lp rp

exprToPred path (UnaryOp op arg _) = do
    argp <- exprToPred (path.op_argL) arg
    return $ pUnaryOp (pOp op) argp

exprToPred path (Dot e ident _) = do
    ep <- exprToPred (path.dot_exprL) e
    case classifyVar ident of
        Expression -> return pExpr'
        (BoundExpression name) -> do
            -- TODO: this doesn't seem right and might explain why
            -- the this test fails in TestRepace.hs:
            -- "x.y" "V1.E1" "E1.V1" "y.x"
            bindExpression name path
            return pExpr'
        Variable -> return $ pDot ep (const True)
        (BoundVariable name) -> do
            bindVariable name (path.dot_attributeL)
            return $ pDot ep (const True)
        Normal -> return $ pDot ep (pIdent ident)

exprToPred path (Lambda params body _) = do
    paramsp <- parametersToPred (path.lambda_argsL) params
    bodyp <- exprToPred (path.lambda_bodyL) body
    return $ pLambda paramsp bodyp

exprToPred path (Call fun args _) = do
    funp <- exprToPred (path.call_funL) fun
    argsp <- argumentsToPred (path.call_argsL) args
    return $ pCall funp argsp

exprToPred path (Subscript subscriptee subscript_expr _) = do
    subscripteep <- exprToPred (path.subscripteeL) subscriptee
    subscript_exprp <- exprToPred (path.subscript_exprL) subscript_expr
    return $ pSubscript subscripteep subscript_exprp

exprToPred path (SlicedExpr expr slices _) = do
    exprp <- exprToPred (path.sliceeL) expr
    slicesp <- slicesToPred (path.slicesL) slices
    return $ pSlicedExpr exprp slicesp

exprToPred path (CondExpr t c f _) = do
    tp <- exprToPred (path.ce_true_branchL) t
    cp <- exprToPred (path.ce_conditionL) c
    fp <- exprToPred (path.ce_false_branchL) f
    return $ pCondExpr tp cp fp

exprToPred path (Paren e _) = pParen <$> exprToPred (path.paren_exprL) e

exprToPred path (StringConversion e _) = pStringConversion <$> exprToPred (path.backquoted_exprL) e

exprToPred path (Tuple es _) =
    pTuple <$> exprsToPred (path.tuple_exprsL) es

exprToPred path (ListComp comp _) = pListComp <$> comprehensionToPred (path.list_comprehensionL) comp

--------------------------------------------------------------------------------

argumentsToPred :: Traversal' QExpr [QArgument] -> [QArgument] -> State RuleState ArgumentsPred
argumentsToPred path args =
    allApply <$> sequence [argumentToPred (path.ix i) arg | (arg, i) <- zip args [0..]]

argumentToPred :: Traversal' QExpr QArgument -> QArgument -> State RuleState ArgumentPred
argumentToPred path (ArgExpr expr _) = pArgExpr <$> exprToPred (path.arg_exprL) expr
argumentToPred path (ArgVarArgsPos expr _) = pArgVarArgsPosExpr <$> exprToPred (path.arg_exprL) expr

--------------------------------------------------------------------------------

parametersToPred :: Traversal' QExpr [QParameter] -> [QParameter] -> State RuleState ParametersPred
parametersToPred path params =
    allApply <$> sequence [parameterToPred (path.ix i) param | (param, i) <- zip params [0..]]

parameterToPred :: Traversal' QExpr QParameter -> QParameter -> State RuleState ParameterPred
parameterToPred path (Param x Nothing Nothing _) =
    case classifyVar x of
        Variable -> return pParam'
        (BoundVariable name) -> do
            bindVariable name (path.param_nameL)
            return pParam'
        Normal -> return $ pParam (pIdent x)

--------------------------------------------------------------------------------

comprehensionExprToPred :: Traversal' QExpr QComprehensionExpr -> QComprehensionExpr -> State RuleState ComprehensionExprPred
comprehensionExprToPred path (ComprehensionExpr expr) = pComprehensionExpr <$> exprToPred (path._ComprehensionExpr) expr
comprehensionExprToPres path (ComprehensionDict dict) = error "ComprehensionDict is unsupported"

--------------------------------------------------------------------------------

comprehensionToPred :: Traversal' QExpr QComprehension -> QComprehension -> State RuleState ComprehensionPred
comprehensionToPred path (Comprehension e for _) = do
    ep <- comprehensionExprToPred (path.comprehension_exprL) e
    forp <- compForToPred (path.comprehension_forL) for
    return $ pComprehension ep forp

compForToPred :: Traversal' QExpr QCompFor -> QCompFor -> State RuleState CompForPred
compForToPred path (CompFor fores ine iter _) = do
    foresp <- exprsToPred (path.comp_for_exprsL) fores
    inep <- exprToPred (path.comp_in_exprL) ine
    iterp <- compIterToPred (path.comp_for_iterL) iter
    return $ pCompFor foresp inep iterp

compIfToPred :: Traversal' QExpr QCompIf -> QCompIf -> State RuleState CompIfPred
compIfToPred path (CompIf if_ iter _) = do
    ifp <- exprToPred (path.comp_ifL) if_
    iterp <- compIterToPred (path.comp_if_iterL) iter
    return $ pCompIf ifp iterp

compIterToPred :: Traversal' QExpr (Maybe QCompIter) -> Maybe QCompIter -> State RuleState (Maybe QCompIter -> Bool)
compIterToPred path (Just (IterFor for _)) = pJustIterFor <$> compForToPred (path._Just.comp_iter_forL) for
compIterToPred path (Just (IterIf if_ _)) = pJustIterIf <$> compIfToPred (path._Just.comp_iter_ifL) if_
compIterToPred path Nothing = return isNothing

slicesToPred :: Traversal' QExpr [QSlice] -> [QSlice] -> State RuleState ([QSlice] -> Bool)
slicesToPred path slices =
    allApply <$> sequence [sliceToPred (path.ix i) slice | (slice, i) <- zip slices [0..]]

sliceToPred :: Traversal' QExpr QSlice -> QSlice -> State RuleState (QSlice -> Bool)
sliceToPred path (SliceProper lower upper (Just stride) _) =
    pSliceProper <$>
        maybeExprToPred (path.slice_lowerL) lower <*>
            maybeExprToPred (path.slice_upperL) upper <*>
                (pJust <$> maybeExprToPred (path.slice_strideL._Just) stride)
sliceToPred path (SliceProper lower upper Nothing _) =
    pSliceProper <$>
        maybeExprToPred (path.slice_lowerL) lower <*>
            maybeExprToPred (path.slice_upperL) upper <*>
                return isNothing

maybeExprToPred :: Traversal' QExpr (Maybe QExpr) -> Maybe QExpr -> State RuleState (Maybe QExpr -> Bool)
maybeExprToPred path (Just e) = pJust <$> exprToPred (path._Just) e
maybeExprToPred path Nothing = return isNothing

--------------------------------------------------------------------------------

pJust :: (a -> Bool) -> Maybe a -> Bool
pJust xp (Just x) = xp x
pJust _ _ = False

pExpr' :: ExprPred
pExpr' _ = True

pIdent :: QIdent -> IdentPred
pIdent x (Ident y _) = ident_string x == y

pVar' :: ExprPred
pVar' Var{} = True
pVar' _ = False

pVar :: IdentPred -> ExprPred
pVar namep (Var name _) = namep name
pVar _ _ = False


pInt :: Integer -> ExprPred
pInt x (Int y _ _) = x == y
pInt _ _ = False


pLongInt :: Integer -> ExprPred
pLongInt x (LongInt y _ _) = x == y
pLongInt _ _ = False

pFloat :: Double -> ExprPred
pFloat x (Float y _ _) = x == y
pFloat _ _ = False

pImaginary :: Double -> ExprPred
pImaginary x (Imaginary y _ _) = x == y
pImaginary _ _ = False

pBool :: Bool -> ExprPred
pBool x (Bool y _) = x == y
pBool _ _  = False

pByteStrings :: [String] -> ExprPred
pByteStrings ss' (ByteStrings ss _) = ss == ss'
pByteStrings _ _ = False

pStrings :: [String] -> ExprPred
pStrings ss' (Strings ss _) = ss == ss'
pStrings _ _ = False

pUnicodeStrings :: [String] -> ExprPred
pUnicodeStrings ss' (UnicodeStrings ss _) = ss == ss'
pUnicodeStrings _ _ = False


pOp :: QOp -> QOp -> Bool
pOp x y = x == y

pBinaryOp :: (QOp -> Bool) -> ExprPred -> ExprPred -> ExprPred
pBinaryOp opp lp rp (BinaryOp op l r _) = opp op && lp l && rp r
pBinaryOp _ _ _ _ = False

pUnaryOp :: (QOp -> Bool) -> ExprPred -> ExprPred
pUnaryOp opp argp (UnaryOp op arg _) = opp op && argp arg
pUnaryOp _ _ _ = False

pDot :: ExprPred -> (QIdent -> Bool) -> ExprPred
pDot ep identp (Dot e ident _) = identp ident && ep e
pDot _ _ _ = False

pLambda :: ([QParameter] -> Bool) -> (QExpr -> Bool) -> ExprPred
pLambda paramsp bodyp (Lambda params body _) = paramsp params && bodyp body
pLambda _ _ _ = False


pCall :: ExprPred -> ArgumentsPred -> ExprPred
pCall funp argsp (Call fun args _) = funp fun && argsp args
pCall _ _ _ = False


pSubscript :: ExprPred -> ExprPred -> ExprPred
pSubscript sp sep (Subscript s se _) = sp s && sep se
pSubscript _ _ _ = False

pSlicedExpr :: ExprPred -> ([QSlice] -> Bool) -> ExprPred
pSlicedExpr exprp slicesp (SlicedExpr expr slices _) = exprp expr && slicesp slices
pSlicedExpr _ _ _ = False

pCondExpr :: ExprPred -> ExprPred -> ExprPred -> ExprPred
pCondExpr tp cp fp (CondExpr t c f _) = tp t && cp c && fp f
pCondExpr _ _ _ _ = False

pSliceProper :: (Maybe QExpr -> Bool) -> (Maybe QExpr -> Bool) -> (Maybe (Maybe QExpr) -> Bool) -> QSlice -> Bool
pSliceProper lowerp upperp stridep (SliceProper lower upper stride _) = lowerp lower && upperp upper && stridep stride
pSliceProper _ _ _ _ = False

pParen :: ExprPred -> ExprPred
pParen ep (Paren e _) = ep e
pParen _ _ = False


pArgExpr :: ExprPred -> ArgumentPred
pArgExpr argp (ArgExpr arg _) = argp arg
pArgExpr _ _  = False

pArgVarArgsPosExpr :: ExprPred -> ArgumentPred
pArgVarArgsPosExpr argp (ArgVarArgsPos arg _) = argp arg
pArgVarArgsPosExpr _ _  = False


pTuple :: [ExprPred] -> QExpr -> Bool
pTuple esp (Tuple es _) = allApply esp es
pTuple _ _ = False


pListComp compp (ListComp comp _) = compp comp
pListComp _ _ = False

pComprehension ep forp (Comprehension e for _) = ep e && forp for

pComprehensionExpr ep (ComprehensionExpr e) = ep e
pComprehensionExpr _ _ = False

pCompFor foresp inep iterp (CompFor fores ine iter _) = inep ine && iterp iter && allApply foresp fores

pCompIf ifp iterp (CompIf if_ iter _) = ifp if_ && iterp iter

pJustIterFor :: CompForPred -> Maybe QCompIter -> Bool
pJustIterFor forp (Just (IterFor for _)) = forp for
pJustIterFor _ _ = False

pJustIterIf :: CompIfPred -> Maybe QCompIter -> Bool
pJustIterIf ifp (Just (IterIf if_ _)) = ifp if_
pJustIterIf _ _ = False

pStringConversion :: ExprPred -> ExprPred
pStringConversion exprp (StringConversion expr _) = exprp expr
pStringConversion _ _ = False

pParam' :: ParameterPred
pParam' (Param _ Nothing Nothing _) = True
pParam' _ = False

pParam :: IdentPred -> QParameter -> Bool
pParam identp (Param ident _ _ _) = identp ident
pParam _ _ = False

--------------------------------------------------------------------------------

allApply fs xs = (length fs == length xs) && all (\(f, x) -> f x) (zip fs xs)
