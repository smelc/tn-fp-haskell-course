{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Course03 where

import Data.Char (isLower)
import Data.Word
import Data.Function ((&))
import Data.List (isInfixOf)

add :: Int -> Int -> Int  -- Type signature
add x y = x + y           -- Syntax: name params = body

tupleAdd :: (Int, Int) -> Int
tupleAdd (x, y) = x + y

add2 :: Int -> Int -> Int
add2 0 y = y
add2 x 0 = x
add2 x y = x + y

data Version =
    Alpha
  | Beta
    -- | Version number of the form "x.y.z"
  | SemVer Int Int Int

isSafe :: Version -> Bool
isSafe Alpha          = False
isSafe Beta           = False 
isSafe (SemVer 1 0 _) = False -- Bug #172, fixed in 1.1.*
isSafe (SemVer 1 1 2) = False -- Bug #175
isSafe _              = True


print :: Version -> String
print v =
  case v of
    Alpha        -> "Alpha"
    Beta         -> "Beta"
    SemVer x y z -> show x ++ "." ++ show y ++ "." ++ show z

lastv :: Version -> Version -> Version
lastv v1 v2 =
  case (v1, v2) of
    (Alpha, _)    -> v2
    (Beta, Alpha) -> Beta
    (Beta, Beta)  -> Beta
    (Beta, SemVer _ _ _) -> v2
    (SemVer x1 _ _, SemVer x2 _ _)   | x1 < x2             -> v2
    (SemVer x1 y1 _, SemVer x2 y2 _) | x1 == x2 && y1 < y2 -> v2
    _ -> error "TODO"

data Sign = Negative | Zero | Positive

sign :: Int -> Sign
sign 0             = Zero
sign n | n < 0     = Negative
       | otherwise = Positive

safeHead :: String -> Maybe Char
safeHead []      = Nothing
safeHead (x : _) = Just x

-- fromMaybe takes the value from a Maybe, or use the default
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a



find :: (a -> Bool) -> Tree a -> Maybe a
find f t = undefined

data Tree a = Node a [Tree a]  -- Discuss alternatives

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


data Operation = Debit Int | Credit Int


wordCount :: [[String]] -> Int
wordCount files = foldr (\words soFar -> (length words) + soFar) 0 files

balance' :: [Operation] -> Int
balance' = foldr (\op soFar -> toInt op + soFar) 0
  where
    toInt = \case Debit x -> -x; Credit x -> x

data Account = MkAccount {
    balance :: Int,
    email :: String,
    name :: Maybe String
  }

-- | Given a list of 'Account', returns the emails of the accounts with more
-- than one million 'balance', 'email' is dubious, and 'name' is omitted.
dubious :: [Account] -> [String]
dubious accounts =
  accounts
    & filter (\account -> account.balance > 1000000)
    & filter (\account -> "ponzi" `isInfixOf` account.email)
    & filter (\account -> case account.name of Nothing -> True; Just _ -> False)
    & map email

-- | @initials "Clément" "Hurlin"@ returns "CH"
initials :: String -> String -> String
initials firstname lastname =
  [extract firstname, extract lastname]
  where
    -- extract returns the initial or '?'
    extract :: String -> Char
    extract name = fromMaybe '?' (safeHead name)
    -- fromMaybe takes the value from a Maybe, or use the default
    fromMaybe :: a -> Maybe a -> a
    fromMaybe a Nothing  = a
    fromMaybe _ (Just a) = a
    -- Returns the first element of a list, or 'Nothing'
    safeHead :: String -> Maybe Char
    safeHead []      = Nothing
    safeHead (x : _) = Just x

-- | 'mkEmail "clement" "hurlin" "tweag" "io"' returns my email
mkEmail firstName lastName domain ext =
  let left = firstName ++ "." ++ lastName in
  let right = domain ++ "." ++ ext in
  left ++ "@" ++ right

