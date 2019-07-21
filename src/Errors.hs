module Errors where

import Text.Parsec (SourcePos)
import Type

data CompilationError =
  CompilationError SourcePos ErrorInfo
  deriving (Show, Eq)

type VarName = String

type Message = String

type ActualType = Type

type ExpectedType = Type

type ExpectedAmount = Int

type ActualAmount = Int

data ErrorInfo
  = RedefinitionError VarName
  | ParseError Message
  deriving (Show, Eq)

data TypeError
  = WrongTypeError ExpectedType ActualType
  | UndefinedNameError VarName
  | WrongArgumentAmountError ExpectedAmount ActualAmount

redefError varName pos = CompilationError pos $ RedefinitionError varName
