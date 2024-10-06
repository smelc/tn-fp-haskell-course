module Course05 where

import Control.Monad.IO.Class
import Data.Char
import Data.List.Split
import Data.Word
import Prelude hiding ((==), Bounded, Enum, Eq, Ordering, Show)

-- | A hostname like @lemonde.fr@ or @google.com@
data Url = Url String Ext

-- | Type representing the extension of an URL
data Ext = Com | Fr

data UrlParsingError = DotError | NonASCII | WrongExt

parseUrl :: String -> Either UrlParsingError Url
parseUrl input =
  splitUrl input
  >>= \(hostStr :: String, extStr :: String) ->
  checkHost hostStr
  >>= \(host :: String) ->
  checkExt extStr
  >>= \(ext  :: Ext) ->
  Right (Url host ext)


splitUrl :: String -> Either UrlParsingError (String, String)
splitUrl input =
  case splitOn "." input of
    host : ext : [] -> Right (host, ext)
    _ -> Left DotError

checkHost :: String -> Either UrlParsingError String
checkHost host | all isAscii host = pure host
               | otherwise = Left NonASCII

checkExt :: String -> Either UrlParsingError Ext
checkExt "com" = Right Com
checkExt "fr"  = Right Fr
checkExt _     = Left WrongExt

monadicParseUrl :: String -> Either UrlParsingError Url
monadicParseUrl input = do
  (hostStr, extStr) <- splitUrl input
  host <- checkHost hostStr
  ext  <- checkExt extStr
  pure (Url host ext)

class (Monoid w, Monad m) => MonadWriter w m | m -> w where

  -- | Produce the output w
  tell :: w -> m ()

  -- Other functions, omitted

writerExample :: MonadWriter String m => m (Maybe Int)
writerExample = do
  a <- pure $ Just 1 -- Could be an IO action
  tell ("Producing a" :: String)
  b <- pure $ Just 2
  tell "Producing b"
  return $ (+) <$> a <*> b

class Monad m => MonadState s m | m -> s where
  -- | Return the state
  get :: m s
  -- | Update the state
  put :: s -> m ()
  -- | Embed an action into the monad
  state :: (s -> (a, s)) -> m a

