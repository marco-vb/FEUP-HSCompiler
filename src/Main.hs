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

  {-
  printTest 22 $ testParser "temperature := 25; humidity := 50; heatIndex1 := temperature * humidity * 10;" == ("","temperature=25,humidity=50,heatIndex1=125")
  printTest 23 $ testParser "total_items := 1000; items_sold := 750; remaining_items := total_items - items_sold;" == ("","total_items=1000,items_sold=750,remaining_items=250")
  printTest 24 $ testParser "long_variable_name := 42; if (long_variable_name == 42) then short_name := 2; else another_long_variable_name := 3;" == ("","long_variable_name=42,short_name=2,another_long_variable_name=0")
  printTest 25 $ testParser "counter := 1; while counter <= 5 do (result := result * counter; counter := counter + 1);" == ("","counter=6,result=120")
  printTest 26 $ testParser "first_variable := 3; second_variable := first_variable * 2; if not (first_variable == second_variable) then first_variable := second_variable; else second_variable := first_variable;" == ("","first_variable=6,second_variable=6")
  printTest 27 $ testParser "variable_one := 5; variable_two := variable_one + 2; variable_three := variable_one * variable_two; while not (variable_two == 0) do (variable_three := variable_three + variable_two; variable_two := variable_two - 1;);" == ("","variable_one=5,variable_two=0,variable_three=63")
  printTest 28 $ testParser "first_long_variable := 3; second_long_variable := 4; result_long_variable := first_long_variable + second_long_variable;" == ("","first_long_variable=3,second_long_variable=4,result_long_variable=7")
  printTest 29 $ testParser "loop_variable := 2; while loop_variable <= 5 do (loop_variable := loop_variable * 2);" == ("","loop_variable=8")
  printTest 30 $ testParser "variable_one := 3; variable_two := variable_one * 2; variable_one := variable_two;" == ("","variable_one=6,variable_two=6")
  printTest 31 $ testParser "main_counter := 2; if main_counter == 2 then nested_counter := 3; else nested_counter := 4;" == ("","main_counter=2,nested_counter=3")
  printTest 32 $ testParser "outer_variable := 5; if not outer_variable == 5 then inner_variable := 6; else inner_variable := 7;" == ("","outer_variable=5,inner_variable=7")
  printTest 33 $ testParser "primary_variable := 3; if primary_variable < 5 then secondary_variable := 2; else (secondary_variable := 4; tertiary_variable := 6;);" == ("","primary_variable=3,secondary_variable=2,tertiary_variable=0")
  -}


printTest :: Int -> Bool -> IO ()
printTest n res = do
  putStrLn $ "Test " ++ show n ++ ": " ++ (if res then "OK" else "Failed")
