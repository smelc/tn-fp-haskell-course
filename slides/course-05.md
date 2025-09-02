class: center, middle

# Functional programming in Haskell

## Lesson 5

<img src="img/tweag_logo_black.png" height="64">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;![TN logo](img/tn.png)

<br/>

Clément Hurlin

[https://github.com/smelc/tn-fp-haskell-course]([https://github.com/smelc/tn-fp-haskell-course)

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```hs
module Course05 where

import Control.Monad.IO.Class
import Data.Char
import Data.List.Split
import Data.Word
import Prelude hiding ((==), Bounded, Enum, Eq, Ordering, Show)
```

```java
class Course05 {
```
-->

---

# Previously

* Typeclasses: open abstractions
* `deriving`: having the compiler generate code
* Phantom types: leveraging the type system for modeling the business' model

In this course:

<center>
  Monads, GADTs and Type Families
</center>

<br/>

---

# Monads

```bash
> :type (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

<!-- exdown-skip 19 20 21 -->
```hs
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

splitUrl  :: String -> Either UrlParsingError (String, String)
checkHost :: String -> Either UrlParsingError String
checkExt  :: String -> Either UrlParsingError Ext
```

???

```hs
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
```

---

# Monads in production: `do`

```bash
> :type (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

```hs
monadicParseUrl :: String -> Either UrlParsingError Url
monadicParseUrl input = do
  (hostStr, extStr) <- splitUrl input
  host <- checkHost hostStr
  ext  <- checkExt extStr
  pure (Url host ext)
```

Compare with:

<!-- exdown-skip -->
```hs
parseUrl :: String -> Either UrlParsingError Url
parseUrl input =
  splitUrl input
  >>= \(hostStr, extStr) ->
  checkHost hostStr
  >>= \host ->
  checkExt extStr
  >>= \ext ->
  Right (Url host ext)
```

---

# Monads: deciphering

<!-- exdown-skip -->
```hs
class Applicative m where
  pure :: a -> m a

  <*> :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
```

1. `>>=` is about sequencing
  1. First compute `m a`
  2. Then use `a`, compute `m b`
1. `a` is generic ➡️ the computation can be anything really (pairs, lists, etc.)
1. `pure` brings a value to the monad

--

<br/>
<center>
  <b>Generic</b> model of interruptible sequential computation
</center>

---

# Monads: Simple Applications

* Computations that may fail
  * Like pretty much any `IO` operation!

<!-- exdown-skip -->
```hs
instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) m f = undefined
```

--

<!-- exdown-skip -->
```hs
instance Monad (Either a) where
  (>>=) :: Either a b -> (b -> Either a c) -> Either a c
  (>>=) e f = undefined
```

???

<!-- exdown-skip -->
```hs
instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) m f =
    case m of
      Nothing -> Nothing
      Just x -> f x

instance Monad (Either a) where
  return :: b -> Either a b
  return = Right

  (>>=) :: Either a b -> (b -> Either a c) -> Either a c
  (Left x) >>= _ = Left x
  (Right x) >>= f = f x
```

---

# Monads, Advanced Applications: `mtl`

* `MonadWriter`: accumulate state

```hs
class (Monoid w, Monad m) => MonadWriter w m | m -> w where

  -- | Produce the output w
  tell :: w -> m ()

  -- Other functions, omitted
```

<br/>

```hs
writerExample :: MonadWriter String m => m (Maybe Int)
writerExample = do
  a <- pure (Just 1) -- Could be an IO action
  tell "Producing a"
  b <- pure Nothing
  tell "Producing b"
  let r = (+) <$> a <*> b
  return r
```

---

# Generalized Algebraic Datatypes (GADTs)

The problem:

<!-- exdown-skip -->
```hs
data Expr a b =
    IntExpr Int
  | BoolExpr Bool
  | PairExpr a b
  | IfThenElseExpr Bool (Expr a b) (Expr a b)

eval :: Expr a b -> ?
```

<center>
🤮
</center>

--

GADTs to the rescue!

<!-- exdown-skip: 7 -->
```hs
data Expr t where
  IntExpr :: Int -> Expr Int
  BoolExpr :: Bool -> Expr Bool
  PairExpr :: Expr a -> Expr b -> Expr (a, b)
  IfThenElseExpr :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
```

<center>
😎
</center>

---

# Type Families

* Type families are type-level functions
* Functions that takes types as inputs and produce types

<center>
Type families' main usage is to work with GADTs
</center>

--

```hs
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

-- | @getData m r@ can return either a value of type @FilePath@
--   or a value of type @Int@!
getData :: Mix -> Role a -> DataKind a
getData mix = \case
  FileRole -> mix.file
  HashRole -> mix.hash
```

---

# Recap

* Monads
  * Generic sequencing operation, supporting interruption
  * Safely bring back sequencing programming to functional programming 👑
* GADTs
  * Existential types within data constructors
  * Fine-grained typing of different constructors
* Type families
  * Uniform treatment of non-uniform types

---

# Recommended reading

* On Hackage:
  * [MonadWriter](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Writer-CPS.html#t:MonadWriter)
  * [MonadState](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Class.html#t:MonadState)
* Type families: https://serokell.io/blog/type-families-haskell

---

# Monads, Advanced Applications: `mtl`

* `MonadState`: pass state around

```hs
class Monad m => MonadState s m | m -> s where
  -- | Return the state
  get :: m s
  -- | Update the state
  put :: s -> m ()
  -- | Embed an action into the monad
  state :: (s -> (a, s)) -> m a
```

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```java
}
```
-->
