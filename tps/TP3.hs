-- Build me with: cabal build TP3.hs
-- Execute me with: cabal run -v0 TP3.hs
-- Load me in the REPL with: cabal repl TP3.hs, then use :r to reload the code upon changing

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "TP3 is running"

-- 1/ Define a type representing URLs you can enter in a browser address bar,
--    i.e. strings of the form:
--    - http://www.google.fr
--    - google.fr
--    - reddit.com/r/haskell
-- 2/ Write a parser from String to your URL type. Its return type must be
--    Either String MyType. The 'Left' case is the error message. Write tests.
-- 3/ We now want to do URL filtering, to grant/forbid access to an URL
--    based on a policy. A filter is a string of the form:
--    - lemonde.fr/emploi
--    - reddit.com/**
--    - reddit.com/*/haskell/**
--    The second filter rules out all URLs of the form reddit.com/suffix, for any 'suffix'
--    The third filter rules out all URLs of the form reddit.com/whatever/haskell/suffix,
--    for any 'whatever' and any 'suffix'. In other words, '*' matches any single
--    segment of the URL while '**' matches all possibles suffixes.
--
--    Write a type for filters
-- 4/ Write a function that taskes an URL and a filter, and returns whether
--    the URL passes the filter. Write tests.
-- 5/ Generalize your filter: allow them to be whitelist, i.e. an URL
--    passes the filter only if matches the filter. Write tests.
--
-- Use https://hoogle.haskell.org/ to find the functions you need