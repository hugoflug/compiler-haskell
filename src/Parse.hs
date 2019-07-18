module Parse
  ( Parse.parse
  ) where

import qualified SyntaxTree as AST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token

import Data.Maybe (isJust)

def =
  javaStyle
    { opStart = oneOf "*+-><=&,!."
    , opLetter = oneOf "*+-><=&,!.length"
    , reservedOpNames =
        ["+", "-", "-", "<", ">", "<=", ">=", "==", "!=", "&&", ",,", "!", ".length"]
    , reservedNames =
        [ "true"
        , "false"
        , "if"
        , "then"
        , "else"
        , "this"
        , "while"
        , "public"
        , "static"
        , "void"
        , "class"
        , "new"
        , "return"
        ]
    }

TokenParser { parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , whiteSpace = m_whiteSpace
            , integer = m_integer
            , brackets = m_brackets
            , symbol = m_symbol
            , commaSep = m_commaSep
            } = makeTokenParser def

r = m_reserved

s = m_symbol

program = do
  m_whiteSpace
  pos <- getPosition
  mainClass' <- mainClass
  classDecls <- many $ try classDecl
  eof
  return $ AST.Program mainClass' classDecls pos

mainClass = do
  pos <- getPosition
  r "class"
  name <- m_identifier
  s "{" >> r "public" >> r "static" >> r "void" >> s "main" >> s "(" >> s "String" >> s "[" >> s "]"
  argName <- m_identifier
  s ")" >> s "{"
  varDecls <- many $ try varDecl
  stmts <- many $ try stmt
  s "}" >> s "}"
  return $ AST.MainClass name argName varDecls stmts pos

classDecl = do
  pos <- getPosition
  r "class"
  name <- ident
  s "{"
  varDecls <- many $ try varDecl
  methodDecls <- many $ try methodDecl
  s "}"
  return $ AST.ClassDecl name varDecls methodDecls pos

methodDecl = do
  pos <- getPosition
  r "public"
  typeNode <- type'
  name <- ident
  formals <- m_parens $ m_commaSep formal
  s "{"
  varDecls <- many $ try varDecl
  stmts <- many $ try stmt
  r "return"
  returnVal <- expr
  s ";"
  s "}"
  return $ AST.MethodDecl typeNode name formals varDecls stmts returnVal pos

formal = do
  pos <- getPosition
  typeNode <- type'
  name <- ident
  return $ AST.GenVarDecl typeNode name AST.Formal pos

varDecl = do
  pos <- getPosition
  typeNode <- type'
  name <- ident
  s ";"
  return $ AST.GenVarDecl typeNode name AST.VarDecl pos

type' = try intArrayType <|> booleanType <|> intType <|> objectType

intArrayType =
  getPosition >>= \pos -> s "int" >> s "[" >> s "]" >> (return $ AST.IntArrayTypeNode pos)

booleanType = getPosition >>= \pos -> s "boolean" >> (return $ AST.BooleanTypeNode pos)

intType = getPosition >>= \pos -> s "int" >> (return $ AST.IntTypeNode pos)

objectType = do
  pos <- getPosition
  name <- m_identifier
  return $ AST.ObjectTypeNode name pos

stmt = try assign <|> try arrayAssign <|> block <|> syso <|> while' <|> if'

assign = do
  pos <- getPosition
  assignee <- ident
  s "="
  newValue <- expr
  s ";"
  return $ AST.Assign assignee newValue pos

arrayAssign = do
  pos <- getPosition
  assignee <- ident
  index <- m_brackets expr
  s "="
  newValue <- expr
  s ";"
  return $ AST.ArrayAssign assignee index newValue pos

block = do
  pos <- getPosition
  s "{"
  stmts <- many $ try stmt
  s "}"
  return $ AST.Block stmts pos

syso = do
  pos <- getPosition
  s "System.out.println"
  expr <- m_parens expr
  s ";"
  return $ AST.Syso expr pos

while' = do
  pos <- getPosition
  r "while"
  cond <- m_parens expr
  statement <- stmt
  return $ AST.While cond statement pos

if' = do
  pos <- getPosition
  r "if"
  cond <- m_parens expr
  thenStmt <- stmt
  maybeElse <- optionMaybe $ r "else"
  if isJust maybeElse
    then do
      elseStmt <- stmt
      return $ AST.If cond thenStmt elseStmt pos
    else return $ AST.IfWithoutElse cond thenStmt pos

expr = buildExpressionParser table term <?> "expression"

term =
  m_parens expr <|> true <|> false <|> this <|> ident <|> intLit <|> try newArray <|> try newObject

true = getPosition >>= \pos -> r "true" >> (return $ AST.True pos)

false = getPosition >>= \pos -> r "false" >> (return $ AST.False pos)

this = getPosition >>= \pos -> r "this" >> (return $ AST.This pos)

intLit = do
  pos <- getPosition
  value <- m_integer
  return $ AST.IntLit value pos

newArray = do
  pos <- getPosition
  r "new"
  r "int"
  size <- m_brackets expr
  return $ AST.NewArray size pos

newObject = do
  pos <- getPosition
  r "new"
  id <- ident
  s "(" >> s ")"
  return $ AST.NewObject id pos

ident = do
  pos <- getPosition
  name <- m_identifier
  return $ AST.Identifier_ $ AST.Identifier name pos

table =
  [ [Prefix not']
  , [Postfix arrayLookup, Postfix (try arrayLength), Postfix (try methodCall)]
  , [Infix mult AssocLeft]
  , [Infix plus AssocLeft, Infix minus AssocLeft]
  , [Infix and' AssocLeft, Infix or' AssocLeft]
  , [Infix gt AssocLeft, Infix get AssocLeft, Infix lt AssocLeft, Infix leqt AssocLeft]
  , [Infix eq AssocLeft, Infix notEq AssocLeft]
  ]

binOp op opNode = do
  pos <- getPosition
  m_reservedOp op
  return $ \l r -> AST.BinaryOp_ $ AST.BinaryOp opNode l r pos

plus = binOp "+" AST.Plus

minus = binOp "-" AST.Minus

mult = binOp "*" AST.Mult

eq = binOp "==" AST.Equal

notEq = binOp "!=" AST.NotEqual

gt = binOp ">" AST.GreaterThan

get = binOp ">=" AST.GreaterOrEqualThan

lt = binOp "<" AST.LessThan

leqt = binOp "<=" AST.LessOrEqualThan

and' = binOp "&&" AST.Equal

or' = binOp "||" AST.NotEqual

not' = do
  pos <- getPosition
  r "!"
  return $ \x -> AST.Not x pos

arrayLength = do
  pos <- getPosition
  s "." >> s "length"
  return $ \array -> AST.ArrayLength array pos

methodCall = do
  pos <- getPosition
  s "."
  methodName <- m_identifier
  args <- m_parens $ m_commaSep Parse.expr
  return $ \obj -> AST.MethodCall obj methodName args pos

arrayLookup = do
  pos <- getPosition
  arrayIndex <- m_brackets Parse.expr
  return $ \array -> AST.ArrayLookup array arrayIndex pos

parse :: SourceName -> String -> Either ParseError AST.Program
parse = Text.Parsec.parse program
