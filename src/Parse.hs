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
import Data.Either.Combinators

def = javaStyle{
                opStart = oneOf "*+-><=&,!"
             , opLetter = oneOf "*+-><=&,!"
             , reservedOpNames = ["+", "-", "-", "<", ">", "<=", ">=", "&&", ",,", "!"]
             , reservedNames = ["true", "false", "if", "then", "else", "this",
                                 "while", "public", "static", "void", "class"]
              }

TokenParser{ parens = m_parens
 , identifier = m_identifier
 , reservedOp = m_reservedOp
 , reserved = m_reserved
 , semiSep1 = m_semiSep1
 , whiteSpace = m_whiteSpace
 , integer = m_integer
 , brackets = m_brackets
 , symbol = m_symbol 
 , commaSep = m_commaSep } = makeTokenParser def

expr = buildExpressionParser table term <?> "expression"

withPos result =
  getPosition >>= \pos -> return $ result pos

term = m_parens Parse.expr
       <|> true
       <|> false
       <|> this
       <|> Parse.identifier
       <|> intLit
       <|> try newArray
       <|> try newObject

true = do
  pos <- getPosition
  m_reserved "true"
  return $ AST.True pos

false = do
  pos <- getPosition
  m_reserved "false"
  return $ AST.False pos

this = do
  pos <- getPosition
  m_reserved "this"
  return $ AST.This pos

intLit = do
  pos <- getPosition
  value <- m_integer 
  return $ IntLit value pos

newArray = do 
  pos <- getPosition
  m_reserved "new"
  m_reserved "int"
  size <- m_brackets m_integer
  return $ NewArray size pos

newObject = do 
  pos <- getPosition
  m_reserved "new"
  id <- Parse.identifier
  m_symbol "()"
  return $ NewObject id pos

identifier = do
  pos <- getPosition
  name <- m_identifier
  return $ Identifier_ $ AST.Identifier name pos

table = [
         [Postfix (try arrayLength), Postfix (try methodCall)] 
       , [Prefix Parse.not]
       , [Infix plus AssocLeft, Infix minus AssocLeft]
      ] 

not = do
  pos <- getPosition
  m_reservedOp "!"
  return $ \x -> Not x pos

plus = do
  pos <- getPosition
  m_reservedOp "+"
  return $ \l r -> BinaryOp_ $ BinaryOp Plus l r pos
  
minus = do
  pos <- getPosition
  m_reservedOp "-"
  return $ \l r -> BinaryOp_ $ BinaryOp Minus l r pos

arrayLength = do
  pos <- getPosition
  m_symbol "."
  m_reservedOp "length"
  return $ \array -> ArrayLength array pos

methodCall = do
  pos <- getPosition
  m_symbol "."
  methodName <- m_identifier
  args <- m_parens $ m_commaSep Parse.expr
  return $ \obj -> MethodCall obj methodName args pos

arrayLookup = do
  pos <- getPosition
  arrayIndex <- m_brackets Parse.expr
  return $ \array -> ArrayLookup array arrayIndex pos

parse :: SourceName -> String -> Either ParseError Expr
parse = Text.Parsec.parse Parse.expr


