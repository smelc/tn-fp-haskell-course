{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Module for live trial and error
module Scratch where

import Data.Char
import Data.Word

class Collection t where
  size :: t a -> Int
  toList :: t a -> [a]

instance Collection [] where
  size = \case [] -> 0; _ : xs -> 1 + size xs
  toList = id

mkEmail :: String -> String -> String -> Either String String
mkEmail user host ext =
  checkUsername user
  >>= \username ->
  checkExt ext
  >>= \extension ->
  Right (username ++ "@" ++ host ++ "." ++ extension)
  where
    checkUsername s =
      if all isLower s
        then Right s
        else Left ("Username should be lowercase, but found: " ++ s)
    checkExt s | s `elem` goodExts = Right s
    checkExt s =
      Left ("Unexpected extension: "
              ++ show s
              ++ ". Expected one of: [\"com\", \"fr\"]")
    goodExts = ["com", "fr"]

data Tree a = Node a [Tree a]

treeFind :: (a -> Bool) -> Tree a -> Maybe a
treeFind f (Node x children) =
  if f x then Just x
  else firstJust (map (treeFind f) children)
  where
    firstJust =
      \case
         [] -> Nothing
         (Just x) : _ -> Just x
         Nothing : xs -> firstJust xs

instance Functor Tree where
  fmap f (Node x children) = Node (f x) (map (fmap f) children)

wordCount :: [[String]] -> Int
wordCount files =
  foldr (\words soFar -> (length words) + soFar) 0 files


data Operation = Debit Word16 | Credit Word16

balance' :: [Operation] -> Word16
balance' = foldr (\op soFar -> toInt op + soFar) 0
  where
    toInt = \case Debit x -> -x; Credit x -> x
