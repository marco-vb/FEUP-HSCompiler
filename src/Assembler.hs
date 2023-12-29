module Assembler (run, createEmptyStack, createEmptyState, stack2Str, state2Str, assemblerTests,
    Code,
    Inst(Push, Add, Mult, Sub, Tru, Fals, Equ, Le, And, Neg, Fetch, Store, Noop, Branch, Loop)
    ) where

import qualified Map
import qualified Stack


-- ===========================================================================================
-- =                                      ASSEMBLER                                          =
-- ===========================================================================================

-- Machine instructions (given)
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

-- Code is a list of instructions (given)
type Code = [Inst]

-- Node of Stack may either be a number or a truth value
data Node = Num Integer | Tval Bool deriving (Show, Eq)

-- Sum two Nodes (error if the any of the two Nodes isn't num)
add :: Node -> Node -> Node
add (Num n1) (Num n2) = Num (n1 + n2)
add _ _ = error "Run-time error"

-- Multiply two Nodes (error if the any of the two Nodes isn't num)
mult :: Node -> Node -> Node
mult (Num n1) (Num n2) = Num (n1 * n2)
mult _ _ = error "Run-time error"

-- Subtract two Nodes (error if the any of the two Nodes isn't num)
sub :: Node -> Node -> Node
sub (Num n1) (Num n2) = Num (n1 - n2)
sub _ _ = error "Run-time error"

-- To compare two Nodes (error if Nodes are different types)
equal :: Node -> Node -> Node
equal (Num n1) (Num n2) = Tval (n1 == n2)
equal (Tval b1) (Tval b2) = Tval (b1 == b2)
equal _ _ = error "Run-time error"

-- To order two Nodes (error if the any of the two Nodes isn't num)
less :: Node -> Node -> Node
less (Num n1) (Num n2) = Tval (n1 < n2)
less _ _ = error "Run-time error"

-- To negate a Node (error if the Node isn't tval)
neg :: Node -> Node
neg (Tval n) = Tval (not n)
neg _ = error "Run-time error"

-- To apply And to two Nodes (error if the any of the two Nodes isn't tval)
andNodes :: Node -> Node -> Node
andNodes (Tval b1) (Tval b2) = Tval (b1 && b2)
andNodes _ _ = error "Run-time error"

-- To convert Node to String
showNode :: Node -> String
showNode (Num n) = show n
showNode (Tval b) = show b

-- NodeStack is a stack of Nodes used by our machine to perform computations
type NodeStack = Stack.Stack Node

-- State of machine is a Map of pairs (key : String, value : Node)
-- Map implemented as a BST
type State = Map.Map String Node

-- Instantiate empty NodeStack
createEmptyStack :: NodeStack
createEmptyStack = Stack.empty

-- Instantiate empty State
createEmptyState :: State
createEmptyState = Map.empty

-- Converts a NodeStack to String
stack2Str :: NodeStack -> String
stack2Str s | Stack.isEmpty s = ""
            | Stack.isEmpty (Stack.pop s) = showNode (Stack.top s)
            | otherwise = showNode (Stack.top s) ++ "," ++ stack2Str (Stack.pop s)

-- Converts a State to String
state2Str :: State -> String
state2Str st = state2StrAux (Map.toList st)

state2StrAux :: [(String, Node)] -> String
state2StrAux [] = ""
state2StrAux (s:ss) | null ss = fst s ++ "=" ++ showNode (snd s)
                    | otherwise = fst s ++ "=" ++ showNode (snd s) ++ "," ++ state2StrAux ss


-- execInst receives a triple (Code, Stack, State)
-- executes the instruction on top of Code
-- and returns the updated triple (RCode, RStack, RState)
-- where RCode is Code except the first instruction
-- RStack and RState are the updated Stack and State after executing the instruction
execInst :: (Code, NodeStack, State) -> (Code, NodeStack, State)

-- Push a Num to the Stack
execInst (Push n : code, stack, state) = (code, Stack.push (Num n) stack, state)

-- Add the two topmost Num of the Stack
execInst (Add : code, stack, state) = 
    (code, Stack.push (add n1 n2) nstack, state)
        where n1 = Stack.top stack
              n2 = Stack.top (Stack.pop stack)
              nstack = Stack.pop (Stack.pop stack)

-- Multiply the two topmost Num of the Stack
execInst (Mult : code, stack, state) = 
    (code, Stack.push (mult n1 n2) nstack, state)
    where n1 = Stack.top stack
          n2 = Stack.top (Stack.pop stack)
          nstack = Stack.pop (Stack.pop stack)

-- Subtract the second topmost Num from the topmost Num of the Stack
execInst (Sub : code, stack, state) = 
    (code, Stack.push (sub n1 n2) nstack, state)
    where n1 = Stack.top stack
          n2 = Stack.top (Stack.pop stack)
          nstack = Stack.pop (Stack.pop stack)

-- Push Tval True to the Stack
execInst (Tru : code, stack, state) = (code, Stack.push (Tval True) stack, state)

-- Push Tval False to the Stack
execInst (Fals : code, stack, state) = (code, Stack.push (Tval False) stack, state)

-- Compare the two topmost Nodes of the Stack
execInst (Equ : code, stack, state) = 
    (code, Stack.push (equal n1 n2) nstack, state)
    where n1 = Stack.top stack
          n2 = Stack.top (Stack.pop stack)
          nstack = Stack.pop (Stack.pop stack)

-- Compare the two topmost Num of the Stack (less than or equal <=)
execInst (Le : code, stack, state) = 
    (code, Stack.push (less n1 n2) nstack, state)
    where n1 = Stack.top stack
          n2 = Stack.top (Stack.pop stack)
          nstack = Stack.pop (Stack.pop stack)

-- Apply And to the two topmost Tval of the Stack
execInst (And : code, stack, state) = 
    (code, Stack.push (andNodes n1 n2) nstack, state)
    where n1 = Stack.top stack
          n2 = Stack.top (Stack.pop stack)
          nstack = Stack.pop (Stack.pop stack)

-- Negate the topmost Tval of the Stack
execInst (Neg : code, stack, state) = 
    (code, Stack.push (neg n) nstack, state)
    where n = Stack.top stack
          nstack = Stack.pop stack

-- Fetch the value of the variable from the State and push it to the Stack
execInst (Fetch s : code, stack, state) = case Map.maplookup s state of
    Nothing   -> error "Run-time error"
    Just n    -> (code, Stack.push n stack, state)

-- Store the value of the topmost Node of the Stack to the State
execInst (Store s : code, stack, state) = (code, nstack, Map.insert s n state)
    where n = Stack.top stack
          nstack = Stack.pop stack

-- Noop
execInst (Noop : code, stack, state) = (code, stack, state)

-- Branch: if the topmost Node of the Stack is Tval True, execute the first Code, otherwise execute the second Code
execInst (Branch c1 c2 : code, stack, state)
    | Stack.top stack == Tval True  = (c1 ++ code, Stack.pop stack, state)
    | Stack.top stack == Tval False = (c2 ++ code, Stack.pop stack, state)
    | otherwise                     = error "Run-time error"

-- Loop: while the topmost Node of the Stack is Tval True, execute the first Code, otherwise execute the second Code
execInst (Loop c1 c2 : code, stack, state) = (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state)

-- If the instruction is not defined, return a run-time error
execInst _ = error "Run-time error"


-- run the machine
run :: (Code, NodeStack, State) -> (Code, NodeStack, State)
run ([], stack, state) = ([], stack, state)
run (code, stack, state) = run (ncode, nstack, nstate)
  where (ncode, nstack, nstate) = execInst (code, stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)


assemblerTests :: IO ()
assemblerTests = do
  printTest 1 $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
  printTest 2 $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
  printTest 3 $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
  printTest 4 $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
  printTest 5 $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
  printTest 6 $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
  printTest 7 $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
  printTest 8 $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
  printTest 9 $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")


printTest :: Int -> Bool -> IO ()
printTest n res = do
  putStrLn $ "Test " ++ show n ++ ": " ++ (if res then "OK" else "Failed")