-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Node of Stack may be number or truth value
data Node = Num Integer | Tval Bool deriving Show

-- Sum two Nodes
add :: Node -> Node -> Node
add (Num n1) (Num n2) = Num (n1 + n2)
add _ _ = error "Run-time error"

-- Multiply two Nodes
mult :: Node -> Node -> Node
mult (Num n1) (Num n2) = Num (n1 * n2)
mult _ _ = error "Run-time error"

-- Subtract two Nodes
sub :: Node -> Node -> Node
sub (Num n1) (Num n2) = Num (n1 - n2)
sub _ _ = error "Run-time error"

-- To compare two Nodes
equal :: Node -> Node -> Node
equal (Num n1) (Num n2) = Tval (n1 == n2)
equal (Tval b1) (Tval b2) = Tval (b1 == b2)
equal _ _ = error "Run-time error"

-- To order two Nodes
less :: Node -> Node -> Node
less (Num n1) (Num n2) = Tval (n1 < n2)
less _ _ = error "Run-time error"

-- To negate a Node
neg :: Node -> Node
neg (Tval n) = Tval (not n)
neg _ = error "Run-time error"

-- To apply And to two Nodes
andNodes :: Node -> Node -> Node
andNodes (Tval b1) (Tval b2) = Tval (b1 && b2)
andNodes _ _ = error "Run-time error"

-- To convert Node to String
showNode :: Node -> String
showNode (Num n) = show n
showNode (Tval b) = show b

-- Stack is just a list of Nodes, head == top
type Stack = [Node]

-- State of machine is just pairs of (Char, Node)
type State = [(String, Node)]

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (s:ss)    | null ss = showNode s
                    | otherwise = showNode s ++ "," ++ stack2Str ss

state2Str :: State -> String
state2Str [] = ""
state2Str st = state2StrAux (sortedState st)

sortedState :: State -> State
sortedState [] = []
sortedState (x:xs) = sortedState smaller ++ [x] ++ sortedState larger
  where
    smaller = [a | a <- xs, fst a < fst x]
    larger = [b | b <- xs, fst b > fst x]

state2StrAux :: State -> String
state2StrAux [] = ""
state2StrAux (s:ss) | null ss = fst s ++ "=" ++ showNode (snd s)
                    | otherwise = fst s ++ "=" ++ showNode (snd s) ++ "," ++ state2StrAux ss


run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (code, stack, state) = run (ncode, nstack, nstate)
  where
    (ncode, nstack, nstate) = execInst (code, stack, state)

execInst :: (Code, Stack, State) -> (Code, Stack, State)
execInst (Push n : code, stack, state) = (code, Num n : stack, state)
execInst (Add : code, stack, state) = (code, add n1 n2 : nstack, state)
  where n1 : n2 : nstack = stack
execInst (Mult : code, stack, state) = (code, mult n1 n2 : nstack, state)
  where n1 : n2 : nstack = stack
execInst (Sub : code, stack, state) = (code, sub n1 n2 : nstack, state)
  where n1 : n2 : nstack = stack
execInst (Tru : code, stack, state) = (code, Tval True : stack, state)
execInst (Fals : code, stack, state) = (code, Tval False : stack, state)
execInst (Equ : code, stack, state) = (code, equal n1 n2 : nstack, state)
  where n1 : n2 : nstack = stack
execInst (Le : code, stack, state) = (code, less n1 n2 : nstack, state)
  where n1 : n2 : nstack = stack
execInst (And : code, stack, state) = (code, andNodes n1 n2 : nstack, state)
  where n1 : n2 : nstack = stack
execInst (Neg : code, stack, state) = (code, neg n : nstack, state)
  where n : nstack = stack
execInst (Fetch s : code, stack, state) 
    | not (any (\x -> fst x == s) state) = error "Run-time error"
    | otherwise = (code, n : stack, state)
    where n = snd (head (filter (\x -> fst x == s) state))
execInst (Store s : code, stack, state) = (code, nstack, (s, n) : nstate)
  where n : nstack = stack
        nstate = filter (\x -> fst x /= s) state
execInst (Noop : code, stack, state) = (code, stack, state)
execInst (Branch c1 c2 : code, Tval True : stack, state) = (c1 ++ code, stack, state)
execInst (Branch c1 c2 : code, Tval False : stack, state) = (c2 ++ code, stack, state)
execInst (Loop c1 c2 : code, stack, state) = 
    (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state)
execInst _ = error "Run-time error"

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples
main :: IO ()
main = do

    putStrLn "Test Assembler"
    print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
    print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
    print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
    --If you test:
    -- testAssembler [Push 1,Push 2,And]
    --You should get an exception with the string: "Run-time error"
    --If you test:
    -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
    --You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--   where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")