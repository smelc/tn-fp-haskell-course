{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Course01 where

import Prelude hiding (reverse)

-- Haskell
evens []                        = []
evens (x : rest) | x mod 2 == 0 = x : evens rest
evens (_ : rest)                = evens rest

-- | The :: notation reads "has type". It is the equivalent of a Java signature.
-- In Java it would be 'static public List<A> reverse(List<A> elems)'
reverse :: [a] -> [a]
reverse [] = []
reverse (x : rest) = (reverse rest) ++ [x] -- Call reverse within reverse

-- map has type :: (a -> b) -> [a] -> [b]
-- in Java it would be 'static public List<B> (Function<A, B> f, List<A> elems)'
-- length has type :: [a] -> Int

toLengths :: [String] -> [Int] -- Type declaration
toLengths xs = map length xs

toLengths2 :: [String] -> [Int]
toLengths2 = map length -- Alternative implementation, partial application

data Version =
    Alpha
  | Beta
    -- | Version number of the form "x.y.z"
  | SemVer Int Int Int

data Driver =
    -- | Car is being driven by a human with a name
    Human String
    -- | Car is on autopilot
    | Autopilot Version

-- | Generic class of containers
class Contains a b where
  get :: a -> b

-- | 'MkPerson "Clément" 39'
data Person = MkPerson String Int

-- | A username like GitHub's @smelc
data Username = Account String

instance Contains Person String where
  get (MkPerson firstName _) = firstName

instance Contains Username String where
  get (Account handle) = "@" ++ handle -- Show GitHub's '@'

toString :: Contains a String => a -> String
toString (whatever :: a) = get whatever

