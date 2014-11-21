{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module QuickCheck (qc) where

import Control.Applicative
import Control.Monad

import Language.Python.Common.AST hiding(annot)
import Test.HUnit.Base

import Test.QuickCheck
import Test.QuickCheck.Random
import Test.QuickCheck.Test

import Quenelle.Lens

-- Some helpers to make the following Arbitrary definitions readable
annot = return ()
emptyl = return []
ident str = Ident str ()
nothing = return Nothing


instance Arbitrary QIdent where
    arbitrary = elements [ident "x", ident "y", ident "z"]

instance Arbitrary QOp where
    arbitrary = elements [
          And ()
        , Or ()
        , Not ()
        , Exponent ()
        , LessThan ()
        , GreaterThan ()
        , Equality ()
        , GreaterThanEquals ()
        , LessThanEquals ()
        , NotEquals  ()
        , NotEqualsV2  ()
        , In ()
        , Is ()
        , IsNot ()
        , NotIn ()
        , BinaryOr ()
        , Xor ()
        , BinaryAnd ()
        , ShiftLeft ()
        , ShiftRight ()
        , Multiply ()
        , Plus ()
        , Minus ()
        , Divide ()
        , FloorDivide ()
        , Invert ()
        , Modulo ()
        , Dot ()
        ]

instance Arbitrary QCompFor where
    arbitrary = oneof [
          CompFor <$> emptyl <*> arbitrary <*> nothing <*> return ()
        , CompFor <$> emptyl <*> arbitrary <*> nothing <*> return ()
        , CompFor <$> emptyl <*> arbitrary <*> arbitrary <*> return ()
        , CompFor <$> arbitrary <*> arbitrary <*> arbitrary <*> return ()
        ]

    shrink (CompFor es e iter ()) = [CompFor es' e' iter' () | (es', e', iter') <- shrink (es, e, iter)]

instance Arbitrary QCompIf where
    arbitrary = oneof [
          CompIf <$> arbitrary <*> nothing <*> return ()
        , CompIf <$> arbitrary <*> nothing <*> return ()
        , CompIf <$> arbitrary <*> arbitrary <*> return ()
        ]

    shrink (CompIf e iter ()) = [CompIf e' iter' () | (e', iter') <- shrink (e, iter)]

instance Arbitrary QCompIter where
    arbitrary = oneof [
          IterFor <$> arbitrary <*> return ()
        , IterIf <$> arbitrary <*> return ()
        ]

    shrink (IterFor for ()) = [IterFor for' () | for' <- shrink for]
    shrink (IterIf if_ ()) = [IterIf if_' () | if_' <- shrink if_]

instance Arbitrary a => Arbitrary (QComprehension a) where
    arbitrary = Comprehension <$> arbitrary <*> arbitrary <*> return ()

    shrink (Comprehension e for ()) = [Comprehension e' for' () | (e', for') <- shrink (e, for)]

instance Arbitrary QArgument where
    arbitrary = oneof [
          ArgExpr <$> arbitrary <*> annot
        , ArgVarArgsPos <$> arbitrary <*> annot
        , ArgVarArgsKeyword <$> arbitrary <*> annot
        , ArgKeyword <$> arbitrary <*> arbitrary <*> annot
        ]
        where annot = return ()

    shrink (ArgExpr e ()) = [ArgExpr e' () | e' <- shrink e]
    shrink (ArgVarArgsPos e ()) = [ArgVarArgsPos e' () | e' <- shrink e]
    shrink (ArgVarArgsKeyword e ()) = [ArgVarArgsKeyword e' () | e' <- shrink e]
    shrink (ArgKeyword k v ()) = [ArgKeyword k' v' () | (k', v') <- shrink (k, v)]

instance Arbitrary QSlice where
    arbitrary = oneof [
          SliceProper <$> arbitrary <*> arbitrary <*> arbitrary <*> annot
        , SliceExpr <$> arbitrary <*> annot
        ]
        where annot = return ()

    shrink (SliceProper l u s ()) = [SliceProper l' u' s' () | (l', u', s') <- shrink (l, u, s)]
    shrink (SliceExpr e ()) = [SliceExpr e' () | e' <- shrink e]
    shrink _ = []


instance Arbitrary QExpr where
    arbitrary = frequency [
          (1, Var <$> arbitrary <*> annot)
        , (1, arbitrary >>= \n -> return $ Int n (show n) ())
        , (1, arbitrary >>= \n -> return $ LongInt n (show n) ())
        , (1, arbitrary >>= \n -> return $ Float n (show n) ())
        , (2, Call <$> arbitrary <*> return [] <*> annot)
        , (1, Call <$> arbitrary <*> arbitrary <*> annot)
        , (1, Subscript <$> arbitrary <*> arbitrary <*> annot)
        , (2, SlicedExpr <$> arbitrary <*> return [] <*> annot)
        , (1, SlicedExpr <$> arbitrary <*> arbitrary <*> annot)
        , (1, CondExpr <$> arbitrary <*> arbitrary <*> arbitrary <*> annot)
        , (1, BinaryOp <$> arbitrary <*> arbitrary <*> arbitrary <*> annot)
        , (1, UnaryOp <$> arbitrary <*> arbitrary <*> annot)
        , (2, Lambda <$> emptyl <*> arbitrary <*> annot)
        , (2, Tuple <$> emptyl <*> annot)
        , (1, Tuple <$> arbitrary <*> annot)
        , (1, Yield <$> arbitrary <*> annot)
        , (1, Generator <$> arbitrary <*> annot)

        , (2, List <$> emptyl <*> annot)
        , (1, List <$> arbitrary <*> annot)

        , (2, Dictionary <$> emptyl <*> annot)
        , (1, Dictionary <$> arbitrary <*> annot)

        , (2, Set <$> emptyl <*> annot)
        , (1, Set <$> arbitrary <*> annot)

        , (1, Paren <$> arbitrary <*> annot)
        ]

    shrink (Call f args ()) = f : [Call f' args' () | (f', args') <- shrink (f, args)]
    shrink (SlicedExpr e ss ()) = e : [SlicedExpr e' ss' () | (e', ss') <- shrink (e, ss)]
    shrink (CondExpr t c f ()) = [t, c, f] ++ [CondExpr t' c' f' () | (t', c', f') <- shrink (t, c, f)]
    shrink (BinaryOp op l r ()) = [l, r] ++ [BinaryOp op l' r' () | (l', r') <- shrink (l, r)]
    shrink (UnaryOp op e ()) = e : [UnaryOp op e' () | e' <- shrink e]
    shrink (Lambda [] e ()) = [e]
    shrink (Tuple [e] ()) = e : [Tuple [e'] () | e' <- shrink e]
    shrink (Tuple xs ()) = [Tuple xs' () | xs' <- shrink xs]
    shrink (Yield Nothing ()) = []
    shrink (Yield (Just e) ()) = e : [Yield (Just e') () | e' <- shrink e]
    -- Try to strip away the generator and look at the generator expression instead
    shrink (Generator c@(Comprehension e (CompFor fes ie _ ()) ()) ()) =
        e : ie : fes ++ [Generator c' () | c' <- shrink c]
    shrink (List [e] ()) = e : [List [e'] () | e' <- shrink e]
    shrink (List es ()) = [List es' () | es' <- shrink es]
    -- Try to strip away the generator and look the the key/values instead
    shrink (Dictionary [(k, v)] ()) = [k, v] ++ shrink k ++ shrink v
    shrink (Dictionary kvs ()) = [Dictionary kvs' () | kvs' <- shrink kvs]
    shrink (Set [e] ()) = e : [Set [e'] () | e' <- shrink e]
    shrink (Set es ()) = [Set es' () | es' <- shrink es]
    shrink (Paren e ()) = e : [Paren e' () | e' <- shrink e]
    shrink _ = []

quenelleArgs n = Args {
      replay = Nothing
    , maxSuccess = n
    , maxDiscardRatio = 10
    -- Exprs are heavily recursive so grow exponentially, so maxSize needs to be very small
    , maxSize = 3
    , chatty = True
    }

qc :: (QExpr -> Bool) -> Int -> String -> Test
qc f n msg = TestCase $ do
    result <- quickCheckWithResult (quenelleArgs n) f
    unless (isSuccess result) $ assertFailure msg
