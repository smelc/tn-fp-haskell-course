module Course04 where

import Control.Monad.IO.Class
import Data.Word
import Prelude hiding ((==), Bounded, Enum, Eq, Ordering, Show)

-- | Types whose values can be compared.
-- Expected to have the following properties:
-- Reflexivity: @x == x@ is @True@
-- Symmetry:    @x == y@ is @y == x@
-- etc.
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  (/=) a b = not (a == b)  -- Default implementation

instance Eq a => Eq (Maybe a) where
  (==) _a _b = undefined


instance Eq a => Eq [a] where -- Build bigger instances from smaller ones
  (==) [] []                         = True
  (==) (a : as) (b : bs) | a == b    = as == bs
  (==) _ _               | otherwise = False

data Ordering = LT | EQ | GT

-- | Totally ordered datatypes
class Ord a where
  compare :: a -> a -> Ordering
  (<=) :: a -> a -> Bool
  -- Other operators: <, >, >=, min, max

-- | Types that can be pretty printed
class Show a where
  show :: a -> String

-- | Types whose values can be enumerated
class Enum a where
  succ :: a -> a
  pred :: a -> a
  -- Other functions

class Bounded a where
  minBound :: a
  maxBound :: a

data Version =
    Alpha
  | Beta
    -- | Version number of the form "x.y.z"
  | SemVer Int Int Int

class (MonadIO m) => MonadLogger m where
  log :: String -> m ()

-- | Generic REST GET interface
class REST a b where
  eval :: (MonadIO m, MonadLogger m) => a -> m b

data PR = PR {
  owner :: String,
  repo :: String,
  number :: Int
}

instance REST PR Bool where
  eval = undefined

data BankAction =
    -- | Une dépense
    Spend Int
    -- | Un gain
  | Earn  Int
    -- | Compte cloturé
  | Close

instance Semigroup BankAction where
  Spend i <> Spend j = Spend (i + j)
  Spend i <> Earn j  = Spend (i - j)
  Close   <> _       = Close
  _       <> Close   = Close
  Earn i  <> Spend j = Earn (i - j)
  Earn i  <> Earn j  = Earn (i + j)

-- Only >= 0 number of votes makes sense
type Nat = Word16

data Reactions = Reactions {
    hearts :: Nat,
    thumbsUp :: Nat,
    thumbsDown :: Nat
  }

data Interval a = Interval {
    start :: a,
    end :: a
  }
  deriving Functor

-- | @makeURL "http" "google.fr" "search/advanced"@ returns
-- @"http://www.google.fr/search/advanced"@
makeURL :: String -> String -> String -> String
makeURL = undefined

newtype Protocol = Protocol String

newtype Hostname = Hostname String

newtype Segments = Segments [String]

makeURL' :: Protocol -> Hostname -> Segments -> String
makeURL' = undefined

data User = User { name :: String, avatar :: FilePath, id :: Int }

-- | @authenticate user password@ tries to authenticate @user@ with @password@
authenticate :: User -> String -> Bool
authenticate = undefined

data Guest
data Authenticated

-- @type@ defines aliases (shortcuts)
type UserWithAuthStatus a = User

authenticate' :: User -> String -> UserWithAuthStatus Authenticated
authenticate' = undefined

