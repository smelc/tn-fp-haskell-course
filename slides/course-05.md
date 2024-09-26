class: center, middle

# Functional programming in Haskell

## Lesson 5

<img src="img/modus-create.png"/ height="96">![TN logo](img/tn.png)

<br/>

Clément Hurlin

[https://github.com/smelc/tn-fp-haskell-course]([https://github.com/smelc/tn-fp-haskell-course)

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```hs
module Course05 where

import Control.Monad.IO.Class
import Data.Char
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
  Mastering the Type System
</center>

<br/>

---

# Monads

```bash
> :type (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

```hs
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
```
---

# User friendly monads: the `do` notation

```bash
> :type (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

```hs
mkEmailSafe' :: String -> String -> String -> Either String String
mkEmailSafe' user host ext = do
  username <- checkUsername user
  extension <- checkExt ext
  pure (username ++ "@" ++ host ++ "." ++ extension)
```

---

# Type Families

---

# Recap

TBD

---

# Recommended reading

TBD

---

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```java
}
```
-->
