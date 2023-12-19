
module Course02 where

import Prelude hiding (Either, filter, Just, Left, Maybe, Nothing, Right, take)

take :: Int -> [a] -> [a]
take n _          | n <= 0 = [] -- The part after the pipe | is a guard
take _ []                  = []
take n (x : rest)          = x : take (n - 1) rest

filter :: (a -> Bool) -> [a] -> [a]
filter _ []                     = []
filter p (x : rest) | p x       = x : filter p rest
                    | otherwise = filter p rest

-- | A type t that is a collection of elements of type 'a'
class Collection t where
  size :: t a -> Int
  isEmpty :: t a -> Bool
  isEmpty x = (size x) == 0 -- Default implementation
  toList :: t a -> [a]

-- | The list type is a collection
instance Collection [] where
  size l = case l of
             [] -> 0
             _ : xs -> 1 + size xs
  toList = id

-- | To require a parameter to implement a class,
-- mention the class to the left of >=
printSize :: Collection t => t a -> IO ()
printSize t = do
  putStrLn (show (size t))

data Maybe a = -- Like Optional in Java
    Nothing
  | Just a

-- | Returns the first element of a list, if any
safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

safeLast :: [a] -> Maybe a
safeLast []  = Nothing
safeLast [x] = Just x
safeLast (_ : rest) = safeLast rest

-- | A new class 'Mappable', which is an extension of 'Collection':
-- instances of 'Mappable' are guaranteed to be instances of 'Collection' too.
class Collection t => Mappable t where
  map :: (a -> b) -> t a -> t b

instance Collection Maybe where
  size = \case Nothing -> 0; Just _ -> 1
  toList = \case Nothing -> []; Just x -> [x]

instance Mappable Maybe where
  map _f = undefined

data Either a b =
    -- | The left case, commonly used as the error case
    Left a
    -- | The right case, commonly used as the valid case
  | Right b

-- | Parse a string using the Read instance. Succeeds if there is
-- exactly one valid result. A Left value indicates a parse error.
readEither :: Read a => String -> Either String a

readEither _ = undefined

data Account = Account {
    balance :: Int,
    email :: String,
    name :: Maybe String
  }

mkAccount :: String -> Account
mkAccount email = Account { balance = 0, email, name = Nothing }

mkAccount2 :: String -> Account
mkAccount2 email = Account { .. } -- Take fields from enclosing scope
  where
    balance = 0
    name = Nothing

setBalance :: Int -> Account -> Account
setBalance n account = account { balance = n } -- functional update

data Interval a = Interval {
  start :: a,
  end :: a
}

instance Collection Interval where
  size _ = undefined
  isEmpty _ = undefined
  toList _ = undefined

instance Mappable Interval where
  map _f _ = undefined

class Semigroup a where
  -- | Associative binary operation
  (<>) :: a -> a -> a

-- | Our 'Mappable' made official!
class Functor where
  fmap :: (a -> b) -> f a -> f b

class Bank x where
  -- | Get credit of account
  getBalance :: x -> String -> Maybe Int
  -- | Does account exist?
  isAccount :: x -> String -> Bool

class Bank x => BankAdmin x where
  -- | Create a new account
  createAccount :: x -> String -> IO ()

-- http://learnyouahaskell.com/input-and-output

main :: IO () -- The program entry point, always
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

