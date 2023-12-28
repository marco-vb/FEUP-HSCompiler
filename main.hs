-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char (isSpace, isDigit, isLower, isAlphaNum)
import Data.Binary.Get (remaining)
import Debug.Trace (traceEvent, traceStack)

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

-- State of machine is just pairs of (String, Node)
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

-- Part 2

data Token = IntToken Integer
           | PlusTok
           | TimesTok
           | MinusTok
           | OpenParenTok
           | ClosedParenTok
           | IfTok
           | ThenTok
           | ElseTok
           | VarTok String
           | AssignTok
           | WhileTok
           | DoTok
           | TrueTok
           | AndTok
           | NotTok
           | IntEqTok           -- ==
           | BoolEqTok          -- =
           | LessOrEqTok        -- <=
           | SemiColonTok
           deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('+': rest) = PlusTok : lexer rest
lexer ('*': rest) = TimesTok : lexer rest
lexer ('-': rest) = MinusTok : lexer rest
lexer ('(': rest) = OpenParenTok : lexer rest
lexer (')': rest) = ClosedParenTok : lexer rest
lexer ('n': 'o': 't': rest) = NotTok : lexer rest
lexer ('a': 'n': 'd': rest) = AndTok : lexer rest
lexer ('i': 'f': rest) = IfTok : lexer rest
lexer ('t': 'h': 'e': 'n': rest) = ThenTok : lexer rest
lexer ('e': 'l': 's': 'e': rest) = ElseTok : lexer rest
lexer ('w': 'h': 'i': 'l': 'e': rest) = WhileTok : lexer rest
lexer ('d': 'o': rest) = DoTok : lexer rest
lexer ('=': '=': rest) = IntEqTok : lexer rest
lexer ('=': rest) = BoolEqTok : lexer rest
lexer ('<': '=': rest) = LessOrEqTok : lexer rest
lexer (':': '=': rest) = AssignTok : lexer rest
lexer ('T': 'r': 'u': 'e': rest) = TrueTok : lexer rest
lexer (';': rest) = SemiColonTok : lexer rest
lexer (c: rest)
  | isSpace c = lexer rest
  | isDigit c = IntToken (read num) : lexer rest'
  | isLower c = VarTok var : lexer rest''
  | otherwise = error ("Invalid character: " ++ [c] ++ " in " ++ (c:rest))
  where (num, rest') = span isDigit (c:rest)
        (var, rest'') = span isAlphaNum (c:rest)

testLexer = lexer "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
-- [VarTok "i",AssignTok,IntToken 10,SemiColonTok,VarTok "fact",AssignTok,IntToken 1,SemiColonTok,WhileTok,OpenParenTok,NotTok,OpenParenTok,VarTok "i",IntEqTok,IntToken 1,ClosedParenTok,ClosedParenTok,DoTok,OpenParenTok,VarTok "fact",AssignTok,VarTok "fact",TimesTok,VarTok "i",SemiColonTok,VarTok "i",AssignTok,VarTok "i",MinusTok,IntToken 1,SemiColonTok,ClosedParenTok,SemiColonTok]


-- Arithmetic expressions
data Aexp = NumExp Integer | VarExp String
            | AddExp Aexp Aexp | MultExp Aexp Aexp | SubExp Aexp Aexp deriving (Show)
-- Boolean expressions
data Bexp = TrueExp
            | LeExp Aexp Aexp | IntEqExp Aexp Aexp | BoolEqExp Bexp Bexp | NotExp Bexp | AndExp Bexp Bexp deriving (Show)
-- Program Statements
data Stm = AssignStm String Aexp
            |  IfStm Bexp [Stm] [Stm] | WhileStm Bexp [Stm] | NoopStm deriving (Show)

type Program = [Stm]

compA :: Aexp -> Code
compA (NumExp n) = [Push n]
compA (VarExp x) = [Fetch x]
compA (AddExp a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (MultExp a1 a2) = compA a2 ++ compA a1 ++ [Mult]
compA (SubExp a1 a2) = compA a2 ++ compA a1 ++ [Sub]    -- a1 - a2 (stack: topmost - second topmost)

compB :: Bexp -> Code
compB TrueExp = [Tru]
compB (LeExp a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (IntEqExp a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (BoolEqExp b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (NotExp b) = compB b ++ [Neg]
compB (AndExp b1 b2) = compB b2 ++ compB b1 ++ [And]

compile :: Program -> Code
compile [] = []
compile (AssignStm var aexp:rest) = compA aexp ++ [Store var] ++ compile rest
compile (IfStm bexp stm1 stm2:rest) = compB bexp ++ [Branch (compile stm1) (compile stm2)] ++ compile rest
compile (WhileStm bexp stm:rest) = Loop (compB bexp) (compile stm) : compile rest
compile (NoopStm:rest) = Noop : compile rest


-- "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
-- [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]


parse :: String -> Program
parse = buildData . lexer

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

buildAexp :: [Token] -> Aexp
buildAexp tokens = case parseSumOrProdOrIntOrPar tokens of
    Just (expr, []) -> expr
    Just _ -> error "Invalid program on buildAexp"
    Nothing -> error "Invalid program on buildAexp"

parseIntOrParen :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParen (IntToken n : restTokens) = Just (NumExp n, restTokens)
parseIntOrParen (OpenParenTok : restTokens1) = case parseSumOrProdOrIntOrPar restTokens1 of
      Just (expr, ClosedParenTok : restTokens2) -> Just (expr, restTokens2)
      Just _ -> Nothing -- no closing parenthesis
      Nothing -> Nothing

parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens = case parseIntOrParen tokens of
    Just (expr1, TimesTok : restTokens1) ->
        case parseProdOrIntOrPar restTokens1 of
            Just (expr2, restTokens2) -> Just (MultExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> result

parseSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrIntOrPar tokens = case parseProdOrIntOrPar tokens of
    Just (expr1, PlusTok : restTokens1) ->
        case parseSumOrProdOrIntOrPar restTokens1 of
            Just (expr2, restTokens2) -> Just (AddExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    Just (expr1, MinusTok : restTokens1) ->
        case parseProdOrIntOrPar restTokens1 of
            Just (expr2, restTokens2) -> Just (SubExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> result



getElseTokens :: [Token] -> ([Token], [Token])
getElseTokens tokens = (elseTokens, restTokens)
  where (_, elseTokens, restTokens) = getElseTokensAux tokens createEmptyStack []

-- assumes there always are parenthesis
-- TODO: Refactor Stack here
getElseTokensAux :: [Token] -> Stack -> [Token] -> (Stack, [Token], [Token])  -- (stack, result, remainder)
getElseTokensAux (OpenParenTok:tokens) stk res = getElseTokensAux tokens (Num 1:stk) (OpenParenTok:res)     -- push to stack and to res (return res and stack)
getElseTokensAux (ClosedParenTok:tokens) stk res = getElseTokensAux tokens (tail stk) (ClosedParenTok:res)   -- pop from stack and push to res
getElseTokensAux (tok:tokens) [] res = ([], res, tok:tokens)             -- stack is empty (parenthesis fully closed) -> return result
getElseTokensAux (tok:tokens) stk res = getElseTokensAux tokens stk (tok:res)  -- if stack non-empty (non-closed parenthesis), token is part of the expression

-- "(x := (3+4); y := 3)"
-- testElseTokens = test [OpenParenTok, VarTok "x", AssignTok, OpenParenTok, IntToken 3, PlusTok, IntToken 4, ClosedParenTok, SemiColonTok, VarTok "y", AssignTok, IntToken 5, SemiColonTok, ClosedParenTok, VarTok "error"]

-- (not True and 2 <= 5 = 3 == 4)
-- (not True) and ((2 <= 5) = (3 == 4))


-- Boolean expression (Bexp) can be:
-- 1. True
-- 2. Aexp <= Aexp
-- 3. Aexp == Aexp
-- 4. not Bexp
-- 5. Bexp = Bexp
-- 6. Bexp and Bexp
-- 7. (Bexp)

buildBexp :: [Token] -> Bexp
buildBexp tokens = case parseTrueLeIEqNotEqAnd tokens of
    Just (expr, []) -> expr
    Just _ -> error "Invalid program BuildBexp invalid result"
    Nothing -> error "Invalid program BuildBexp nothing"


parseTrueLeIEqNotEqAnd :: [Token] -> Maybe (Bexp, [Token])
parseTrueLeIEqNotEqAnd tokens = case parseTrueLeIEqNotEq tokens of
    Just (expr1, AndTok : restTokens1) ->
        case parseTrueLeIEqNotEqAnd restTokens1 of
            Just (expr2, restTokens2) -> Just (AndExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> result

parseTrueLeIEqNotEq :: [Token] -> Maybe (Bexp, [Token])
parseTrueLeIEqNotEq tokens = case parseTrueLeIEqNot tokens of
    Just (expr1, BoolEqTok : restTokens1) ->
        case parseTrueLeIEqNotEq restTokens1 of
            Just (expr2, restTokens2) -> Just (BoolEqExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> result

parseTrueLeIEqNot :: [Token] -> Maybe (Bexp, [Token])
parseTrueLeIEqNot (NotTok : rest) = case parseTrueLeIEqNot rest of
    Just (expr, restTokens) -> Just (NotExp expr, restTokens)
    Nothing -> Nothing
parseTrueLeIEqNot tokens = parseTrueLeIeq tokens

parseTrueLeIeq :: [Token] -> Maybe (Bexp, [Token])
parseTrueLeIeq tokens = case parseSumOrProdOrIntOrPar tokens of   -- TODO
    Just (expr1, IntEqTok : restTokens1) ->
        case parseSumOrProdOrIntOrPar restTokens1 of
            Just (expr2, restTokens2) -> Just (IntEqExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> parseTrueLe tokens  -- TODO

parseTrueLe :: [Token] -> Maybe (Bexp, [Token])
parseTrueLe tokens = case parseSumOrProdOrIntOrPar tokens of      -- TODO
    Just (expr1, LessOrEqTok : restTokens1) ->
        case parseSumOrProdOrIntOrPar restTokens1 of
            Just (expr2, restTokens2) -> Just (LeExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> parseTrue tokens  -- if cannot parseAexp or there is no LessOrEqTok, call function below ? (TODO)

-- parseTrue takes care of True and of parenthesis
parseTrue :: [Token] -> Maybe (Bexp, [Token])
parseTrue (TrueTok : restTokens) = Just (TrueExp, restTokens)
parseTrue (OpenParenTok : restTokens1) = case parseTrueLeIEqNotEqAnd restTokens1 of
    Just (expr, ClosedParenTok : restTokens2) -> Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing


buildData :: [Token] -> [Stm]
buildData [] = []
buildData (SemiColonTok:tokens) = buildData tokens
buildData ((VarTok var):AssignTok:tokens) = AssignStm var (buildAexp aexp) : buildData rest
  where (aexp, rest) = break (== SemiColonTok) tokens

buildData (IfTok:tokens) = IfStm (buildBexp bexp) (buildData thenTokens) (buildData elseTokens) : buildData rest
    where (bexp, withThenTokens) = break (== ThenTok) tokens
          (thenTokens, withElseTokens) = break (== ElseTok) (tail withThenTokens)
          afterElseTokens = tail withElseTokens
          (elseTokens, rest) =
                if head afterElseTokens == OpenParenTok then
                    -- break (== ClosedParenTok) (tail afterElseTokens) -- TODO: recursive function to resolve parenthesis
                    getElseTokens afterElseTokens
                else
                    break (== SemiColonTok) afterElseTokens

buildData (WhileTok:tokens) = WhileStm (buildBexp bexp) (buildData doTokens) : buildData rest
    where (bexp, withDoTokens) = break (== DoTok) tokens
          (doTokens, rest) =
                if head (tail withDoTokens) == OpenParenTok then
                    -- break (== ClosedParenTok) (tail (tail withDoTokens)) -- TODO: recursive function to resolve parenthesis
                    getElseTokens (tail withDoTokens)
                else
                    break (== SemiColonTok) (tail withDoTokens)

buildData _ = error "Invalid program on buildData"



-- buildData (IfTok:rest) = IfStm (buildBexp bexp) (head s1) (head s2) : buildData (tail (tail rest2))
--   where (bexp, rest1) = break (== ThenTok) rest
--         (s, rest2) = break (== ElseTok) (tail rest1)

-- buildData (WhileTok:rest) = WhileStm (buildBexp rest1) (head s1) : buildData (tail rest1)
--   where (s1, rest1) = break (== DoTok) rest
-- buildData _ = error "Invalid program"


{-
i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);

[VarTok "i",AssignTok,IntToken 10,SemiColonTok,
VarTok "fact",AssignTok,IntToken 1,SemiColonTok,

WhileTok,OpenParenTok,NotTok,OpenParenTok,VarTok "i",IntEqTok,IntToken 1,ClosedParenTok,ClosedParenTok,
DoTok,

OpenParenTok,
VarTok "fact",AssignTok,VarTok "fact",TimesTok,VarTok "i",SemiColonTok,
VarTok "i",AssignTok,VarTok "i",MinusTok,IntToken 1,SemiColonTok,
ClosedParenTok,

SemiColonTok]

x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;
"x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)"
-}
