module Type where

data Type = 
    BooleanType | 
    IntArrayType | 
    IntType | 
    ObjectType { name :: String } | 
    VoidType