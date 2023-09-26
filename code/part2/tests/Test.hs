-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [test1, test2, test3, test4, test5, test6, test8, test9, test10, test11, test12, test13, test14, test15] where
  -- Test of showExp
  test1 = ("test1", showExp (Sub (Add (Cst 2) (Cst 2)) (Mul (Cst 2) (Cst 2)))     == "((2)+(2))-((2)*(2))")
  test2 = ("test2", showExp (Div (Pow (Cst 10) (Cst 2)) (Cst 2))                  == "((10)^(2))`div`(2)")
  -- Test of evalSimple
  test3 = ("test3", evalSimple (Sub (Add (Cst 2) (Cst 2)) (Mul (Cst 2) (Cst 2)))  == 0)
  test4 = ("test4", evalSimple (Div (Pow (Cst 10) (Cst 2)) (Cst 2))               == 50)
  -- Test of evalFull
  test5 = (("test5", evalFull (Add (Let "a" (Cst 10) (Mul (Var "a") (Var "a"))) 
           (Let "a" (Cst 2) (Var "a"))) initEnv                                   == 102))
  test6 = (("test6", evalFull (If (Div (Cst 10) (Cst 2)) (Pow (Cst 2) (Cst 2)) 
           (Sub (Cst 10) (Cst 5))) initEnv                                        == 5))
  test7 = (("test7", evalFull (Sum "x" (Cst 1) (Add (Cst 2) (Cst 2)) (Mul (Var "x") 
                          (Var "x"))) initEnv                                     == 30))
  -- Test of evalErr
  test8  = ("test8", evalErr (Var "x") initEnv                                    == Left (EBadVar "x"))
  test9  = ("test9", evalErr (Var "a") (extendEnv "a" 1 initEnv)                  == Right 1)
  test10 = ("test10", evalErr (Div (Var "x") (Var "x")) (extendEnv "x" 0 initEnv) == Left (EDivZero))
  test11 = ("test11", evalErr (Div (Var "x") (Var "x")) (extendEnv "x" 4 initEnv) == Right 1)
  test12 = ("test12", evalErr (Pow (Cst 2) (Sub (Cst 2) (Cst 4))) initEnv         == Left (ENegPower))
  test13 = ("test13", evalErr (Pow (Cst 2) (Cst 2)) initEnv                       == Right 4)
  test14 = (("test14", evalErr (If (Cst 0) (Mul (Cst 2) (Cst 2)) 
            (Add (Cst 4) (Cst 2))) initEnv                                        == Right 4))
  test15 = (("test4", evalErr (Add (Let "a" (Cst 10) (Mul (Var "a") (Var "a"))) 
            (Let "a" (Cst 2) (Var "a"))) initEnv                                  == Right 102))

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
