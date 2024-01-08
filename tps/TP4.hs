-- Build me with: cabal build TP4.hs
-- Execute me with: cabal run -v0 TP4.hs
-- Load me in the REPL with: cabal repl TP4.hs, then use :r to reload the code upon changing

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import System.Process
import Test.QuickCheck

-- The goal of this TP is to implement an evaluator for arithmetic expressions.
-- Here is the incremental list of objectives:
-- - Define a type of arithmetic expressions. It should support:
--   - Int constants
--   - Additions of two expressions
--   - Negation of an expression
--   - Feel free to add constructs
-- - Define an evaluator for an expression. I.e. given your type of expressions
--   Expr, define the function eval :: Expr -> Int
-- - Make 'Expr' an instance of the 'Show' typeclass
-- - Use this instance to compare your implementation of 'eval' with
--   the result of evaluating your expressions with python3, using the 'pyEval'
--   function below. Write a quickCheck test to compare the two implementations.

-- | pyEval calls python3 and make it execute a python statement.
-- For example, pyEval "2 * 3" returns "6"
-- For 'readProcess' documentation, see:
-- https://hackage.haskell.org/package/process-1.6.13.2/docs/System-Process.html#v:readProcess
pyEval :: String -> IO String
pyEval expr = do
  readProcess "python3" ["-c", "print(" ++ expr ++ ", end='')"] ""

main :: IO ()
main = do
  pyResult <- pyEval "1 + 3"
  putStrLn ("pyEval \"1 + 3\" returned: " ++ pyResult)
