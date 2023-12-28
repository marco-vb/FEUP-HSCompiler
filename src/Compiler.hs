module Compiler (
    compile,
    Program,
    Aexp(NumExp, VarExp, AddExp, MultExp, SubExp),
    Bexp(TrueExp, FalseExp, LeExp, IntEqExp, BoolEqExp, NotExp, AndExp),
    Stm(AssignStm, IfStm, WhileStm)
    ) where

import Assembler (
    Code,
    Inst(Push, Add, Mult, Sub, Tru, Fals, Equ, Le, And, Neg, Fetch, Store, Noop, Branch, Loop))

type Program = [Stm]

-- Arithmetic expressions
data Aexp = NumExp Integer | VarExp String
            | AddExp Aexp Aexp | MultExp Aexp Aexp | SubExp Aexp Aexp deriving (Show)

-- Boolean expressions
data Bexp = TrueExp | FalseExp | LeExp Aexp Aexp | IntEqExp Aexp Aexp
            | BoolEqExp Bexp Bexp | NotExp Bexp | AndExp Bexp Bexp deriving (Show)

-- Program Statements
data Stm = AssignStm String Aexp
            |  IfStm Bexp [Stm] [Stm] | WhileStm Bexp [Stm] deriving (Show)

-- To compile arithmetic expressions we use compA
compA :: Aexp -> Code
compA (NumExp n) = [Push n]
compA (VarExp x) = [Fetch x]
compA (AddExp a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (MultExp a1 a2) = compA a2 ++ compA a1 ++ [Mult]
compA (SubExp a1 a2) = compA a2 ++ compA a1 ++ [Sub]    -- (a1 - a2) (stack: topmost - second topmost)


-- To compile boolean expressions we use compB
compB :: Bexp -> Code
compB TrueExp = [Tru]
compB FalseExp = [Fals]
compB (LeExp a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (IntEqExp a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (BoolEqExp b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (NotExp b) = compB b ++ [Neg]
compB (AndExp b1 b2) = compB b2 ++ compB b1 ++ [And]


-- Higher level compile function that handles all statements
compile :: Program -> Code
compile [] = []
compile (AssignStm var aexp:rest) = compA aexp ++ [Store var] ++ compile rest
compile (IfStm bexp stm1 stm2:rest) = compB bexp ++ [Branch (compile stm1) (compile stm2)] ++ compile rest
compile (WhileStm bexp stm:rest) = Loop (compB bexp) (compile stm) : compile rest
