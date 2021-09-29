-- | Module for live trial and error
module Scratch where

import Data.Char

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
