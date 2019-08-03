module JVMByteCode where

type ClassName = String

type Name = String

type SuperClass = String

type TypeDescriptor = String

type MaxStack = Integer

type MaxLocals = Integer

type VarNo = Integer

type Label = Int

type Size = Integer

data MethodKind
  = Static
  | Instance
  deriving (Show, Eq)

data JVMClass =
  JVMClass Name SuperClass [JVMField] [JVMMethod]
  deriving (Show, Eq)

data JVMMethod =
  JVMMethod Name TypeDescriptor MethodKind MaxStack MaxLocals [JVMInstruction]
  deriving (Show, Eq)

data JVMField =
  JVMField Name TypeDescriptor
  deriving (Show, Eq)

data JVMInstruction
  = MethodRefInstruction MethodRefInstruction ClassName Name TypeDescriptor
  | FieldRefInstruction FieldRefInstruction ClassName Name TypeDescriptor
  | VarNoInstruction VarNoInstruction VarNo
  | Labelled LabelInstruction Label
  | Iconst_0
  | Iconst_1
  | Dup
  | Pop
  | Label Label
  | Aload_0
  | Swap
  | Return
  | Ireturn
  | Areturn
  | Ldc_wInt Int
  | Ldc_wString String
  | Newarray Size
  | Iadd
  | Isub
  | Imul
  | Ixor
  | Arraylength
  | Iaload
  | Iastore
  | New ClassName
  | Nop
  deriving (Show, Eq)

data MethodRefInstruction
  = Invokevirtual
  | Invokespecial
  deriving (Show, Eq)

data FieldRefInstruction
  = Putfield
  | Getfield
  | Getstatic
  deriving (Show, Eq)

data VarNoInstruction
  = Istore
  | Astore
  | Iload
  | Aload
  deriving (Show, Eq)

data LabelInstruction
  = Goto
  | Ifeq
  | If_icmpgt
  | If_icmpge
  | If_icmple
  | If_icmplt
  | Ifne
  | If_acmpeq
  | If_icmpeq
  | If_acmpne
  | If_icmpne
  deriving (Show, Eq)
