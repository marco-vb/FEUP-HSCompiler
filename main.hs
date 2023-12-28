-- PFL 2023/24 - Haskell practical assignment

import Parser (parse)
import Assembler (run, createEmptyStack, createEmptyState, stack2Str, state2Str)
import Compiler (compile)

main :: IO ()
main = do
  putStrLn "Test Parser"
  -- Examples:
  print $ testParser "x := 5; x := x - 1;" == ("","x=4")
  print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
  print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
  print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
  print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
   where (_,stack,state) = Assembler.run(compile (parse programCode), createEmptyStack, createEmptyState)

