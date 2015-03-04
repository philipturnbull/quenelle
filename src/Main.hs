module Main where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Options.Applicative hiding (Success)
import System.Console.ANSI

import Language.Python.Common.AST
import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST
import Language.Python.Version2.Parser

import Quenelle.File
import Quenelle.Lens
import Quenelle.Match
import Quenelle.Normalize
import Quenelle.Parse
import Quenelle.Replace
import Quenelle.Rule

oldSGR = [SetColor Foreground Dull Red]
newSGR = [SetColor Foreground Dull Green]

printMatchHeader :: StatementSpan -> ExprLocation -> IO ()
printMatchHeader stmt loc = do
    putStrLn $ locstr ++ replicate (headerwidth - locwidth) '='
    putStrLn $ prettyText stmt
    putStrLn $ replicate headerwidth '='
    where locstr = "====[ " ++ show loc ++ " ]"
          locwidth = length locstr
          headerwidth = 60

printMatch :: StatementSpan -> ExprReplacement -> ExprMatch -> IO ()
printMatch stmt replacement match = do
    with oldSGR $ putStr "  - "
    putStrLn $ prettyText (exprMatchExpr match)
    with newSGR $ putStr "  + "
    putStrLn $ prettyText result
    putStrLn "\n"
    where result = doReplacement replacement match
          with color a = setSGR color >> a >> setSGR []

runRule :: ExprRule -> ExprReplacement -> TopLevelExpr -> IO ()
runRule rule replacement (TopLevelExpr stmt loc expr) =
    unless (null matches) $ do
        printMatchHeader stmt loc
        mapM_ (printMatch stmt replacement) matches
    where matches = matchExprRule rule expr

quenelleFile :: ExprRule -> ExprReplacement -> FilePath -> IO ()
quenelleFile rule replacement filename = do
    mexprs <- moduleExprsFromFile filename
    case mexprs of
        Left err -> print err
        Right exprs -> mapM_ (runRule rule replacement) exprs

quenelle :: Opts -> IO ()
quenelle opts = do
    result <- parseRuleFromFile $ ruleFilename opts
    case result of
        ParseError err -> print err
        RuleError err -> print err
        ReplacementError err -> print err
        Success rule replacement -> mapM_ (quenelleFile rule replacement) (sourceFilenames opts)

data Opts = Opts {
    ruleFilename :: FilePath,
    sourceFilenames :: [FilePath]
    }

main :: IO ()
main = execParser opts >>= quenelle
    where parser =
            Opts <$> argument str (metavar "rule-file")
                 <*> some (argument str (metavar "source.py"))
          opts = info (helper <*> parser) mempty
