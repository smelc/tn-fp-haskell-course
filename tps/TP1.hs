-- Execute me with: cabal run -v0 TP1.hs

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck

-- | A type for thumb up and thumb down emojis
data ThumbType = Up | Down
  deriving (Show) -- So that the type can be printed

main :: IO ()
main = do
  putStrLn ("Haskell is " ++ (show Up))
  -- Once you have written the corresponding functions, uncomment this:
  -- quickCheck propMyNeg
  -- quickCheck propMyAnd
  -- quickCheck propMyFilter

-- | Define the Boolean type. Don't use 'Bool' as the name to avoid clashing
-- with the Prelude's definition.
-- data MyBool = ...
--   deriving (Eq, Show, Generic) -- to be able to 1/ compare values 2/ print them 3/ generate them

-- | Define negation over 'MyBool'
-- myNeg :: MyBool -> MyBool

-- | Define conjunction over 'MyBool'
-- myAnd :: MyBool -> MyBool -> MyBool
-- myAnd b1 b2 = ...

-- | Define a type of list. Here 'a' is the type of elements. As it's a generic
-- list, it should work over all element types.
-- data MyList a = ...

-- | Define a function that returns whether a 'MyList' is empty
-- isEmpty :: MyList a -> MyBool
-- isEmpty l = ...

-- | Define list filtering. Given a list [e0, e1, ..., en]; it returns
-- the sublist of elements that satisfies the predicate given as first argument.
-- For example myFilter myNeg '[MyTrue, MyFalse, MyFalse]' should return
-- '[MyFalse, MyTrue, MyTrue]'.
-- myFilter :: (a -> MyBool) -> MyList a -> MyList a
-- myFilter pred ... = ...

-- Testing
--
-- Once you have implemented the functions above, uncomment the following
-- code and execute your file, it will test your functions automatically.
-- The command line to execute the file is given in the header above.

instance Arbitrary MyBool where
  arbitrary = genericArbitraryU
  shrink = genericShrink

leq :: MyBool -> MyBool -> Bool
leq MyFalse MyTrue = True
leq MyTrue MyFalse = False
leq _ _ = True

propMyNeg :: MyBool -> Bool
propMyNeg b = myNeg b /= b

propMyAnd :: MyBool -> MyBool -> Bool
propMyAnd b1 b2 = (myAnd b1 b2 `leq` b1) && (myAnd b1 b2 `leq` b2)

instance Arbitrary a => Arbitrary (MyList a) where
  arbitrary = ofList <$> arbitrary

propMyFilter :: MyList a -> MyBool
propMyFilter xs = isEmpty (myFilter (\_ -> False) xs)

