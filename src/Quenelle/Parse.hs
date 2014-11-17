module Quenelle.Parse (
    ParseResult(..),

    parseRuleFromFile,
    parseRuleFromString
) where

import Control.Applicative

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Prim

import Quenelle.Replace
import Quenelle.Rule

data ParseResult =
      Success ExprRule ExprReplacement
    | ParseError String
    | RuleError String
    | ReplacementError String

instance Show ParseResult where
    show (Success _ _) = "success"
    show (ParseError err) = "Error parsing rule: " ++ err
    show (RuleError err) = "Error parsing matching: " ++ err
    show (ReplacementError err) = "Error parsing replacement: " ++ err


pythonExpr :: GenParser Char st String
pythonExpr = many1 $ satisfy (/= '\n')

rule :: GenParser Char () (String, String)
rule = do
    string "replace:"
    rulestr <- between newline newline pythonExpr
    string "with:"
    replacementstr <- between newline newline pythonExpr
    return (rulestr, replacementstr)

parseRuleFromString :: FilePath -> String -> ParseResult
parseRuleFromString filename contents =
    case parse rule filename contents of
        Left err -> ParseError $ show err
        Right (rulestr, replacementstr) ->
            case parseExprRule rulestr of
                Left err -> RuleError $ show err
                Right rule -> case parseExprReplacement replacementstr of
                    Left err -> ReplacementError $ show err
                    Right replacement -> Success rule replacement

parseRuleFromFile :: String -> IO ParseResult
parseRuleFromFile filename = parseRuleFromString filename <$> readFile filename
