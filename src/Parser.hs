module Parser (parse) where

import Data.Char (isSpace, isDigit, isLower, isAlphaNum)

import qualified Stack (Stack, empty, isEmpty, push, pop, top)
import Compiler (
    Program,
    Stm(AssignStm, IfStm, WhileStm),
    Aexp(NumExp, VarExp, AddExp, MultExp, SubExp),
    Bexp(TrueExp, FalseExp, LeExp, IntEqExp, BoolEqExp, NotExp, AndExp))

parse :: String -> Program
parse = buildData . lexer

-- ===========================================================================================
-- =                                        Lexer                                            =
-- ===========================================================================================

-- Tokens for lexer
data Token = IntToken Integer
           | PlusTok            -- +
           | TimesTok           -- *
           | MinusTok           -- -
           | OpenParenTok       -- (
           | ClosedParenTok     -- )
           | IfTok              -- if
           | ThenTok            -- then
           | ElseTok            -- else
           | VarTok String      -- variable
           | AssignTok          -- :=
           | WhileTok           -- while
           | DoTok              -- do
           | TrueTok            -- True
           | FalseTok           -- False
           | AndTok             -- and
           | NotTok             -- not
           | IntEqTok           -- ==
           | BoolEqTok          -- =
           | LessOrEqTok        -- <=
           | SemiColonTok       -- ;
           deriving (Show, Eq)

lexer :: String -> [Token]  -- converts a string into a list of tokens
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
lexer ('F': 'a': 'l': 's' : 'e': rest) = FalseTok : lexer rest
lexer (';': rest) = SemiColonTok : lexer rest
lexer (c: rest)
  | isSpace c = lexer rest  -- ignore spaces
  | isDigit c = IntToken (read num) : lexer rest'   -- get digits and convert to integer
  | isLower c = VarTok var : lexer rest''           -- starts w/ lowercase letter -> variable
  | otherwise = error ("Invalid character: " ++ [c] ++ " in " ++ (c:rest))
  where (num, rest') = span isDigit (c:rest)        -- get all digits
        (var, rest'') = span isAlphaNum (c:rest)    -- get all alphanumeric characters


-- ===========================================================================================
-- =                                 Build Data (Statements)                                 =
-- ===========================================================================================

buildData :: [Token] -> [Stm]
buildData [] = []
buildData (SemiColonTok:tokens) = buildData tokens  -- ignore semicolons

-- assigment statement
buildData ((VarTok var):AssignTok:tokens) = AssignStm var (buildAexp aexp) : buildData rest
  where (aexp, rest) = break (== SemiColonTok) tokens

-- if statement
buildData (IfTok:tokens) = IfStm (buildBexp bexp) (buildData thenTokens) (buildData elseTokens) : buildData rest
    where (bexp, withThenTokens) = break (== ThenTok) tokens
          afterThenTokens = tail withThenTokens
          (thenTokens, withElseTokens) = 
                if head afterThenTokens == OpenParenTok then
                  getBetweenParenTokens afterThenTokens 
                else
                    break (== SemiColonTok) afterThenTokens
          afterElseTokens =
                if head withElseTokens == SemiColonTok then
                  drop 2 withElseTokens   -- drop SemiColonTok and ElseTok
                else
                  tail withElseTokens     -- drop ElseTok
          (elseTokens, rest) =
                if head afterElseTokens == OpenParenTok then    -- if parenthesis
                    getBetweenParenTokens afterElseTokens       -- statements between parenthesis
                else
                    break (== SemiColonTok) afterElseTokens     -- only 1 statement w/o parenthesis

-- while statement
buildData (WhileTok:tokens) = WhileStm (buildBexp bexp) (buildData doTokens) : buildData rest
    where (bexp, withDoTokens) = break (== DoTok) tokens
          (doTokens, rest) =
                if head (tail withDoTokens) == OpenParenTok then
                    getBetweenParenTokens (tail withDoTokens)
                else
                    break (== SemiColonTok) (tail withDoTokens)

buildData _ = error "Invalid program on buildData"



-- ===========================================================================================
-- =                          Parse and build arithmetic expressions                         =
-- ===========================================================================================
buildAexp :: [Token] -> Aexp    -- build arithmetic expressions
buildAexp tokens = case parseSumOrHigher tokens of
    Just (expr, []) -> expr
    Just _ -> error "Invalid program on buildAexp"
    Nothing -> error "Invalid program on buildAexp"

-- parsing functions called according to precendence
-- last function called handles operations with highest precedence

-- Handles Sums and Subtractions and calls parseProdOrHigher to handle higher precedence
parseSumOrHigher :: [Token] -> Maybe (Aexp, [Token])
parseSumOrHigher tokens = case parseProdOrHigher tokens of
    Just (expr1, PlusTok : restTokens1) ->
        case parseSumOrHigher restTokens1 of
            Just (expr2, restTokens2) -> Just (AddExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    Just (expr1, MinusTok : restTokens1) ->
        case parseSumOrHigher restTokens1 of
            Just (expr2, restTokens2) -> Just (SubExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> result

-- Handles Products and calls pareIntOrParen to handle higher precedence than products
parseProdOrHigher :: [Token] -> Maybe (Aexp, [Token])
parseProdOrHigher tokens = case parseIntParenVar tokens of
    Just (expr1, TimesTok : restTokens1) ->
        case parseProdOrHigher restTokens1 of
            Just (expr2, restTokens2) -> Just (MultExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> result


-- highest precedence: integer, variable, or expression between parenthesis
parseIntParenVar :: [Token] -> Maybe (Aexp, [Token])
parseIntParenVar (IntToken n : restTokens) = Just (NumExp n, restTokens)
parseIntParenVar (VarTok var : restTokens) = Just (VarExp var, restTokens)
parseIntParenVar (OpenParenTok : restTokens1) = case parseSumOrHigher restTokens1 of
      Just (expr, ClosedParenTok : restTokens2) -> Just (expr, restTokens2)
      Just _ -> Nothing         -- no closing parenthesis or not parseable expression
      Nothing -> Nothing
parseIntParenVar _ = Nothing     -- not Int, Var, or OpenParen: not Aexp


-- ===========================================================================================
-- =                          Parse and build boolean expressions                            =
-- ===========================================================================================
-- Boolean expression (Bexp) can be:
-- 1. True or False
-- 2. (Bexp), 3. Aexp <= Aexp, 4. Aexp == Aexp
-- 5. not Bexp, 6. Bexp = Bexp, 7. Bexp and Bexp

buildBexp :: [Token] -> Bexp    -- build boolean expressions
buildBexp tokens = case parseAndOrHigher tokens of
    Just (expr, []) -> expr
    Just _ -> error "Invalid program BuildBexp invalid result"
    Nothing -> error "Invalid program BuildBexp nothing"

-- Handles "And" and calls parseBoolEqOrHigher to handle higher precedence
parseAndOrHigher :: [Token] -> Maybe (Bexp, [Token])
parseAndOrHigher tokens = case parseBoolEqOrHigher tokens of
    Just (expr1, AndTok : restTokens1) ->
        case parseAndOrHigher restTokens1 of
            Just (expr2, restTokens2) -> Just (AndExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> result

-- Handles "=" (Bool equality) and calls parseNotOrHigher to handle higher precedence
parseBoolEqOrHigher :: [Token] -> Maybe (Bexp, [Token])
parseBoolEqOrHigher tokens = case parseNotOrHigher tokens of
    Just (expr1, BoolEqTok : restTokens1) ->
        case parseBoolEqOrHigher restTokens1 of
            Just (expr2, restTokens2) -> Just (BoolEqExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> result

-- Handles "not" and calls parseIntEqOrHigher to handle higher precedence
parseNotOrHigher :: [Token] -> Maybe (Bexp, [Token])
parseNotOrHigher (NotTok : rest) = case parseNotOrHigher rest of
    Just (expr, restTokens) -> Just (NotExp expr, restTokens)
    Nothing -> Nothing
parseNotOrHigher tokens = parseIntEqOrHigher tokens

-- Handles "==" (Int equality) and calls parseLeOrHigher to handle higher precedence
-- Calls parseSumOrHigher to parse arithmetic expressions
parseIntEqOrHigher :: [Token] -> Maybe (Bexp, [Token])
parseIntEqOrHigher tokens = case parseSumOrHigher tokens of
    Just (expr1, IntEqTok : restTokens1) ->
        case parseSumOrHigher restTokens1 of
            Just (expr2, restTokens2) -> Just (IntEqExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> parseLeOrHigher tokens

-- Handles "<=" and calls parseTrueParen to handle higher precedence
-- Uses parseSumOrHigher to parse arithmetic expressions
parseLeOrHigher :: [Token] -> Maybe (Bexp, [Token])
parseLeOrHigher tokens = case parseSumOrHigher tokens of
    Just (expr1, LessOrEqTok : restTokens1) ->
        case parseSumOrHigher restTokens1 of
            Just (expr2, restTokens2) -> Just (LeExp expr1 expr2, restTokens2)
            Nothing -> Nothing
    result -> parseTrueParen tokens  -- if cannot parseAexp or there is no LessOrEqTok

-- parseTrueParen handles True, False and parenthesis
-- Calls parseAndOrHigher to parse expressions inside parenthesis
parseTrueParen :: [Token] -> Maybe (Bexp, [Token])
parseTrueParen (TrueTok : restTokens) = Just (TrueExp, restTokens)
parseTrueParen (FalseTok : restTokens) = Just (FalseExp, restTokens)
parseTrueParen (OpenParenTok : restTokens1) = case parseAndOrHigher restTokens1 of
    Just (expr, ClosedParenTok : restTokens2) -> Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing


type ParenthesisStack = Stack.Stack Char
type ResultTokens = [Token]
type RemainderTokens = [Token]

-- assumes the expression always has parenthesis (must start with OpenParenTok)
getBetweenParenTokens :: [Token] -> (ResultTokens, RemainderTokens)
getBetweenParenTokens tokens = (elseTokens, restTokens)
  where (restTokens, _, elseTokens) = getBetweenParenTokensAux tokens Stack.empty []

-- Receives tokens to process, stack and current result
-- Returns remainder, stack, and result
getBetweenParenTokensAux :: RemainderTokens -> ParenthesisStack -> ResultTokens
    -> (RemainderTokens, ParenthesisStack, ResultTokens)
-- reverse the result since tokens are inserted in reversed order
-- no more tokens, return result
getBetweenParenTokensAux [] stk res = ([], Stack.empty, reverse res)

-- push parenthesis to stack
getBetweenParenTokensAux (OpenParenTok:tokens) stk res = 
    getBetweenParenTokensAux tokens (Stack.push '(' stk) res

-- pop parenthesis from stack
getBetweenParenTokensAux (ClosedParenTok:tokens) stk res = 
    getBetweenParenTokensAux tokens (Stack.pop stk) res


-- stack is empty (parenthesis fully closed) -> return result
-- if stack non-empty (non-closed parenthesis), token is part of the expression
getBetweenParenTokensAux (tok:tokens) stk res    
    | Stack.isEmpty stk = (tok:tokens, Stack.empty, reverse res)
    | otherwise         = getBetweenParenTokensAux tokens stk (tok:res)

