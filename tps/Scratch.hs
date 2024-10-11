{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
-- | Module for live trial and error
module Scratch where

import Data.Char
import Data.List.Split
import Data.Word
import Control.Monad.Writer

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
  >>= \host ->
  checkExt extStr
  >>= \ext ->
  Right (Url host ext)

monadicParseUrl :: String -> Either UrlParsingError Url
monadicParseUrl input = do
  (hostStr :: String, extStr :: String) <- splitUrl input
  host <- checkHost hostStr
  ext <- checkExt extStr
  pure (Url host ext)

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

data MyMaybe a = MyNothing | MyJust a
  deriving Functor

instance Applicative MyMaybe where
  pure :: a -> MyMaybe a
  pure = MyJust

  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  (<*>) mf x =
    case (mf, x) of
      (MyNothing, _) -> MyNothing
      (_, MyNothing) -> MyNothing
      (MyJust f, MyJust x) -> MyJust $ f x

instance Monad MyMaybe where
  return :: a -> MyMaybe a
  return = MyJust

  (>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
  (>>=) m f =
    case m of
      MyNothing -> MyNothing
      MyJust x -> f x

data MyEither a b = MyLeft a | MyRight b

instance Functor (MyEither a) where
  fmap _ (MyLeft x) = MyLeft x
  fmap f (MyRight x) = MyRight (f x)

instance Applicative (MyEither a) where
  pure = MyRight
  (MyLeft x) <*> _ = MyLeft x
  (MyRight f) <*> x = fmap f x

instance Monad (MyEither a) where
  return :: b -> MyEither a b
  return = MyRight

  (>>=) :: MyEither a b -> (b -> MyEither a c) -> MyEither a c
  (MyLeft x) >>= _ = MyLeft x
  (MyRight x) >>= f = f x

monadWriterExample :: MonadWriter String m => m (Maybe Int)
monadWriterExample = do
  a <- pure (Just 1) -- Could be an IO action
  tell "Producing a"
  b <- pure $ Nothing
  tell "Producing b"
  return $ (+) <$> a <*> b

data Expr t where
  IntExpr :: Int -> Expr Int
  BoolExpr :: Bool -> Expr Bool
  Pair :: Expr a -> Expr b -> Expr (a, b)
  IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a


eval :: Expr a -> a
eval = \case
  IntExpr x -> x
  BoolExpr x -> x
  Pair a b -> (eval a, eval b)
  IfThenElse cond a b -> if eval cond then eval a else eval b

-- data Expr a b =
--     IntExpr Int
--   | BoolExpr Bool
--   | PairExpr a b
--   | IfThenElseExpr Bool (Expr a b) (Expr a b)

data Mix = MkMix { file :: FilePath, hash :: Int }

-- Phantom types
data File
data Hash

data Role a where
  FileRole :: Role File
  HashRole :: Role Hash

type family DataKind a where
  DataKind File = FilePath
  DataKind Hash = Int

getData :: Mix -> Role a -> DataKind a
getData mix = \case
  FileRole -> mix.file
  HashRole -> mix.hash