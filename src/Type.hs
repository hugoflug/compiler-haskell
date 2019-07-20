module Type where

data Type
  = BooleanType
  | IntArrayType
  | IntType
  | ObjectType String
  | VoidType
  deriving (Show, Eq)
