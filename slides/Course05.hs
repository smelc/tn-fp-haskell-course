module Course05 where

import Control.Monad.IO.Class
import Data.Char
import Data.Word
import Prelude hiding ((==), Bounded, Enum, Eq, Ordering, Show)

mkEmailSafe :: String -> String -> String -> Either String String
mkEmailSafe user host ext =
  checkUsername user
  >>= \(username :: String) ->
  checkExt ext
  >>= \(extension :: String) ->
  Right (username ++ "@" ++ host ++ "." ++ extension)

checkUsername :: String -> Either String String
checkUsername s | all isLower s = Right s
                | otherwise     = Left ("Username should be lowercase, but found: " ++ s)
checkExt :: String -> Either String String
checkExt =
  \case
    "com" -> Right "com"
    "fr"  -> Right "fr"
    s     -> Left ("Unexpected extension: "
                     ++ show s
                     ++ ". Expected one of: [\"com\", \"fr\"]")

mkEmailSafe' :: String -> String -> String -> Either String String
mkEmailSafe' user host ext = do
  username <- checkUsername user
  extension <- checkExt ext
  pure (username ++ "@" ++ host ++ "." ++ extension)

