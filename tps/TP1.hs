-- Build me with: cabal build TP1.hs
-- Execute me with: cabal run -v0 TP1.hs
-- Load me in the REPL with: cabal repl TP1.hs, then use :r to reload the code upon changing

--
-- In this file, you need to replace the @undefined@ calls by real code.
--
-- Resource for the syntax of functions: https://learnyouahaskell.github.io/syntax-in-functions.html

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Prelude hiding (and, drop, length, not, take, not)

-- | A type for thumb up and thumb down emojis
data ThumbType = Up | Down
  deriving (Show) -- So that the type can be printed

main :: IO ()
main = do
  putStrLn ("Haskell is " ++ (show Up))
  quickCheck propNegAnd -- Tests the property 'propNegAnd'
  quickCheck (propLength :: [Int] -> Bool) -- Specialize function, so that data can be generated
  quickCheck propSumRecSumFold

-- | Write a negation function over Bool: 'neg'
neg :: Bool -> Bool
neg _ = undefined

-- | Write the conjunction function over Bool: 'and'
and :: Bool -> Bool -> Bool
and _ _ = undefined

-- | A function stating a property of 'neg' and 'and'
propNegAnd :: Bool -> Bool
propNegAnd b = neg (and b (neg b))

-- | Write a function computing the length of a list
length :: [a] -> Int
length _ = undefined

-- | write a function that states a property of 'length', for any input
-- list.
propLength :: [a] -> Bool
propLength _ = undefined

-- | Write a function taking the first 'n' elements of a list. The function
-- should be total.
take :: Int -> [a] -> [a]
take _ _ = undefined

-- | Write a function taking the suffix of a list, after the first 'n' elements.
drop :: Int -> [a] -> [a]
drop _ _ = undefined

-- | Write a recursive function that sums the elements of a list
sumRec :: [Int] -> Int
sumRec _ = undefined

-- | Write a non-recursive function that sums the elements of a list, using
-- the foldr function: https://hoogle.haskell.org/?hoogle=foldr
sumFold :: [Int] -> Int
sumFold _ = undefined

-- | Write a function stating a relation between 'sumRec' and 'sumFold'
propSumRecSumFold :: [Int] -> Bool
propSumRecSumFold _ = undefined

-- | Write the fmap instance for 'Maybe'
fmapMaybe :: (a -> b) -> (Maybe a) -> (Maybe b)
fmapMaybe _f = undefined

-- | Write the map instance for 'List'. Don't use the standard library's 'map' function
fmapMaybeList :: (a -> b) -> [a] -> [b]
fmapMaybeList = undefined