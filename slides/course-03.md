class: center, middle

# Functional programming in Haskell

## Lesson 3

![Tweag logo](img/tweag.png) ![TN logo](img/tn.png)

<br/>

Clément Hurlin

[https://github.com/smelc/tn-fp-haskell-course]([https://github.com/smelc/tn-fp-haskell-course)

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```hs
{-# Language NamedFieldPuns #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}


module Course03 where

import Data.Char (isLower)
import Data.Function ((&))
import Data.List (isInfixOf)
```

```java
class Course03 {
```
-->

---

# Previously

* How to define **types**
* How to define **constants** (in the REPL)

Now:

<center>
Define functions, i.e. things that transforms values into values.
</center>

**Forget**:

- mutable variables
- pointers and references
- dynamic dispatch

**Remember**:

- Simple high school equations
- Functions as seen in high school (_fonctions affines_ anyone?)

---

# Functions

### Plain function

```hs
add :: Int -> Int -> Int  -- Type signature
add x y = x + y           -- Syntax: name params = body
```

```java
public static int add(int x, int y) { return x + y; } 
```

???

Note the difference with `tupleAdd`:

```hs
tupleAdd :: (Int, Int) -> Int
tupleAdd (x, y) = x + y
```

---

# Pattern matching function

<!-- exdown-skip  -->
```hs
add2 :: Int -> Int -> Int
add2 0 y = y
add2 x 0 = x
```

<center>What's the problem?</center>

--

```shell
    Pattern match(es) are non-exhaustive
    In an equation for ‘add2’:
        Patterns not matched:
            p q where p is not one of {0}
                      q is not one of {0}
```

<center>The function is not <i>total</i></center>

Totality is the property that, for any input, the function returns
normally: without returning an exception or crashing the program.

--

```hs
add2 :: Int -> Int -> Int
add2 0 y = y
add2 x 0 = x
add2 x y = x + y
```

<center>Order matters!</center>

???

Highlight how the last member of the type signature is asymmetrical:
it's an output. Other members are inputs.

---

# Pattern matching function

Back in course 1:

```hs
data Version =
    Alpha
  | Beta
    -- | Version number of the form "x.y.z"
  | SemVer Int Int Int
```

Pattern matching mirrors the type's definition:

```hs
isSafe :: Version -> Bool
isSafe Alpha          = False
isSafe Beta           = False 
isSafe (SemVer 1 0 _) = False -- Bug #172, fixed in 1.1.*
isSafe (SemVer 1 1 2) = False -- Bug #175
isSafe _              = True
```

---

# Pattern matching expression: `case`

<!-- exdown-skip 1 2 3 4  -->
```hs
-- Where 'show' below comes from, aka toString()
class Show a where
  -- | Print a value
  show :: a -> String

print :: Version -> String
print v =
  case v of
    Alpha        -> "Alpha"
    Beta         -> "Beta"
    SemVer x y z -> show x ++ "." ++ show y ++ "." ++ show z
```

```hs
lastv :: Version -> Version -> Version
lastv v1 v2 =
  case (v1, v2) of
    (Alpha, _)    -> v2
    (Beta, Alpha) -> Beta
    (Beta, Beta)  -> Beta
    (Beta, SemVer _ _ _) -> v2
    (SemVer x1 _ _, SemVer x2 _ _)   | x1 < x2             -> v2
    (SemVer x1 y1 _, SemVer x2 y2 _) | x1 == x2 && y1 < y2 -> v2
    _ -> error "I'm too lazy"
```

???

- Remove the `_` catch all, look at the error message
- What is the type of `error`?

---

# Functions with guards

```hs
data Sign = Negative | Zero | Positive

sign :: Int -> Sign
sign 0             = Zero
sign n | n < 0     = Negative
       | otherwise = Positive
```

---

# Where

```hs
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
```

- The order in the `where` clause does not matter
- It is idiomatic to have a small function body and a long
  `where` clause

???

Ask for the generalization of the type of `safeHead`

Indentation matters 😢:

- Nested expressions should be intended
- function body is intended w.r.t. the function name
- members of the `where` clause are intended from the `where`

---

# Execution model

Remember equations from high school?

```hs
safeHead :: String -> Maybe Char
safeHead []      = Nothing
safeHead (x : _) = Just x

-- fromMaybe takes the value from a Maybe, or use the default
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a
```

Use the definitions as [rewriting rules](https://en.wikipedia.org/wiki/Rewriting#Term_rewriting_systems):

```shell
   fromMaybe '?' (safeHead "Clément")
```

--

```shell
→ fromMaybe '?' (safeHead ('C' : "lément"))
→ fromMaybe '?' (Just 'C')
→ 'C'
```

---

# Let

Contrary to `where`, `let` introduces an order between declarations:

```hs
-- | 'mkEmail "clement" "hurlin" "tweag" "io"' returns my email
mkEmail firstName lastName domain ext =
  let left = firstName ++ "." ++ lastName in
  let right = domain ++ "." ++ ext in
  left ++ "@" ++ right
```

Its syntax is: `let varName = expression in expression`

???

- What is the type of `mkEmail`?
  - How does inference work?

---

# Composition

Because functions are so central in functional programming, it is crucial combine them easily.

```shell
> import Data.Function
> :type (&)
(&) :: a -> (a -> b) -> b
```

- `(&)` means it is an operator (like `+`, `-`, etc.), so it is written between its arguments: `x & f`

???

Ask whether they see the relationship with function application

---

# Partial application

```shell
> :type map
map :: (a -> b) -> [a] -> [b]
> :type show
show :: Show a => a -> String
```

- What is the type of `map show`?

--

</br>

```shell
:type (<)
(<) :: Ord a => a -> a -> Bool
```

- What is the type of `(<) 3`?

--

</br>

When writing functions:
<center>
- Order arguments so that partial application makes sense
</center>

---

# Composition

```shell
> :type filter
filter :: (a -> Bool) -> [a] -> [a]
> :type map
map :: (a -> b) -> [a] -> [b]
```

```hs
data Account = Account {
    balance :: Int,
    email :: String,
    name :: Maybe String
  }

-- | Given a list of 'Account', returns the ones with more than one
-- million 'balance', 'email' is dubious, and 'name' is omitted.
dubious :: [Account] -> [String]
dubious accounts =
  filter (\Account{balance} -> balance > 1000000) accounts
    & filter (\Account{email} -> "ponzi" `isInfixOf` email)
    & filter (\Account{name} -> case name of Nothing -> True; Just _ -> False)
    & map email
```

???

- If you were to make this type support multiple currencies, how
  would you do it?
- What constructs would you use in an imperative language to implement
  `dubious`?
  - Would you rather have 3 loops or one loop?

---

# More composition

```shell
> import Data.Functor
> :type (<&>)
(<&>) :: Functor f => f a -> (a -> b) -> f b
> :type fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

- Given an element of type `a` wrapped in something (`f`), and a function from `a` to `b`, apply the function inside the wrapping.

- What can be `f`?

???

```shell
> import Data.List.Extra
> :type upper
upper :: String -> String
> applyUpper = fmap upper
> applyUpper (Just "foo")
> applyUpper (Right "foo" :: Either Int String)
```

---

# More composition

```shell
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
  where
    checkUsername :: String -> Either String String
    checkUsername s =
      if all isLower s
        then Right s
        else Left ("Username should be lowercase, but found: " ++ s)
    checkExt :: String -> Either String String
    checkExt "com" = Right "com"
    checkExt "fr" = Right "fr"
    checkExt s =
      Left ("Unexpected extension: "
              ++ show s
              ++ ". Expected one of: [\"com\", \"fr\"]")
```

???

If done at this point and time remains:

- Explain `foldr`
- Explain things defined in terms of `foldr`:
  - `sum`
  - `and :: [Bool] -> Bool`
  - `any :: (a -> Bool) -> [a] -> Bool`

---

# Recap

How to build functions from:

- Pattern matching (`case ... of `)
- Guards (`f x | cond x = ...`)

How to compose functions:

- `(&)`: chain
- `(<&>)`: chain in presence of wrapping
- `(>>=)`: chain computations while in a special context

---

# Recommended Reading

- http://learnyouahaskell.com/syntax-in-functions
- http://learnyouahaskell.com/higher-order-functions

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```java
}
```
-->
