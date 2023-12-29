-- PFL 2023/24 - Haskell practical assignment

import Parser (parse)
import Assembler (run, createEmptyStack, createEmptyState, stack2Str, state2Str, assemblerTests, Code, 
    Inst(Push, Add, Mult, Sub, Tru, Fals, Equ, Le, And, Neg, Fetch, Store, Noop, Branch, Loop))
import Compiler (compile)

main :: IO ()
main = do
  putStrLn "Test Parser"
  printTest 1 $ testParser "x := 5; x := x - 1;" == ("","x=4")
  printTest 2 $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
  printTest 3 $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
  printTest 4 $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
  printTest 5 $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
  printTest 6 $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
  printTest 7 $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

-- To help testing the assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- To help testing the parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
   where (_,stack,state) = Assembler.run(compile (parse programCode), createEmptyStack, createEmptyState)

additionalTests :: IO ()  -- Additional test cases
additionalTests = do
  printTest 1 $ testParser "x := 5; x := x - 1;" == ("","x=4")
  printTest 2 $ testParser "a := 10; b := 20; c := a + b * 2;" == ("","a=10,b=20,c=50")
  printTest 3 $ testParser "x := 5; if x == 5 then y := 2; else y := 3;" == ("","x=5,y=2")
  printTest 4 $ testParser "x := 2; while x <= 5 do (x := x * 2);" == ("","x=8")
  printTest 5 $ testParser "a := 3; b := a * 2; if not (a == b) then a := b; else b := a;" == ("","a=6,b=6")
  printTest 6 $ testParser "x := 5; y := x + 2; z := x * y; while not (y == 0) do (z := z + y; y := y - 1;);" == ("","x=5,y=0,z=63")
  printTest 7 $ testParser "a := 3; b := 4; c := a + b;" == ("","a=3,b=4,c=7")
  printTest 8 $ testParser "x := 2; while x <= 5 do (x := x * 2);" == ("","x=8")
  printTest 9 $ testParser "a := 3; b := a * 2; a := b;" == ("","a=6,b=6")
  

  printTest 10 $ testParser "x := 5; if (x == 5) then y := 2; else y := 3;" == ("","x=5,y=2")
  printTest 11 $ testParser "x := 2; while x <= 5 do (x := x * 2);" == ("","x=8")
  printTest 12 $ testParser "a := 3; b := a * 2; if not (a == b) then a := b; else b := a;" == ("","a=6,b=6")
  printTest 13 $ testParser "x := 5; y := x + 2; z := x * y; while not (y == 0) do (z := z + y; y := y - 1;);" == ("","x=5,y=0,z=63")
  printTest 14 $ testParser "a := 3; b := 4; c := a + b;" == ("","a=3,b=4,c=7")
  printTest 16 $ testParser "a := 3; b := a * 2; a := b;" == ("","a=6,b=6")
  printTest 17 $ testParser "x := 2; if x == 2 then y := 3; else y := 4;" == ("","x=2,y=3")
  printTest 18 $ testParser "a := 5; if not a == 5 then b := 6; else b := 7;" == ("","a=5,b=7")
  printTest 19 $ testParser "x := 3; if x <= 5 then y := 2; else (y := 4; z := 6;);" == ("","x=3,y=2")
  printTest 20 $ testParser "x:=0; if(x+3)==3 then y:=2;else y:=1;" == ("","x=0,y=2")
  printTest 21 $ testParser "x:=0; if x+3==3 then y:=2;else y:=1;" == ("","x=0,y=2")


-- Prints the result of a test in format "Test n: OK" or "Test n: Failed"
printTest :: Int -> Bool -> IO ()
printTest n res = do
  putStrLn $ "Test " ++ show n ++ ": " ++ (if res then "OK" else "Failed")
