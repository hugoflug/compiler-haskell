module Parse
    ( 
      Parse.parse
    ) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import SyntaxTree as AST

def = javaStyle{
                opStart = oneOf "*+-><=&|"
              , opLetter = oneOf "*+-><=&|"
              , reservedOpNames = ["+", "-", "-", "<", ">", "<=", ">=", "&&", "||"]
              , reservedNames = ["true", "false", "if", "then", "else",
                                 "while", "public", "static", "void", "class"]
              }

TokenParser{ parens = m_parens
  , identifier = m_identifier
  , reservedOp = m_reservedOp
  , reserved = m_reserved
  , semiSep1 = m_semiSep1
  , whiteSpace = m_whiteSpace } = makeTokenParser def

exprParser = buildExpressionParser table term <?> "expression"

table = [ [Prefix Parse.not]
        , [Infix plus AssocLeft]
        ]

term = m_parens exprParser
       <|> (m_reserved "true" >> return (AST.True 0))
       <|> (m_reserved "false" >> return (AST.False 0))

not = m_reservedOp "-" >> return (\x -> Not x 0)
plus = m_reservedOp "+" >> return (\l r -> BinaryOp_ (BinaryOp Plus l r 0))

parse :: SourceName -> String -> Either ParseError Expr
parse = Text.Parsec.parse exprParser


