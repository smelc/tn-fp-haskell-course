class: center, middle

# Functional programming in Haskell

## Lesson 3

![Tweag logo](img/tweag.png) ![Modus logo](img/modus-create.png)

<br/>

![TN logo](img/tn.png)

Clément Hurlin

[https://github.com/smelc/tn-fp-haskell-course]([https://github.com/smelc/tn-fp-haskell-course)

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```hs
{-# Language LambdaCase #-}
{-# Language NamedFieldPuns #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}


module Course03 where

import Data.Char (isLower)
import Data.Word
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

```bash
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
-- | The 'Show' class, akin to the 'toString()' method in Java
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

```bash
   fromMaybe '?' (safeHead "Clément")
```

--

```bash
→ fromMaybe '?' (safeHead ('C' : "lément"))
→ fromMaybe '?' (Just 'C')
→ 'C'
```

---

# Let

Contrary to `where`, `let` can be used in the body of functions:

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

Because functions are so central in functional programming, it is crucial
to combine them easily.

```bash
> import Data.Function
> :type (&)
(&) :: a -> (a -> b) -> b
```

- `(&)` means it is an operator (like `+`, `-`, etc.), so it is written between its arguments: `x & f`,
  in infix position.

???

Ask whether they see the relationship with function application

---

# Partial application

```bash
> :type map
map :: (a -> b) -> [a] -> [b]
> :type show
show :: Show a => a -> String
```

- What is the type of `map show`?

--

</br>

```bash
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

```bash
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

-- | Given a list of 'Account', returns the emails of the accounts with more
-- than one million 'balance', 'email' is dubious, and 'name' is omitted.
dubious :: [Account] -> [String]
dubious accounts =
  accounts
    & filter (\Account{balance} -> balance > 1000000)
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

# Functional toolbox: recursion

To solve problems functionally:

- Recursion: divide a problem into smaller subproblems
- Fold: iterate over data, accumulate a value along the way

<!-- exdown-skip 6 7 -->
```hs
data Tree a = Node a [Tree a]

find :: (a -> Bool) -> Tree a -> Maybe a
find f t = undefined

map :: (a -> b) -> Tree a -> Tree b
map f t = undefined
```

???

```hs
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
```

- Ask whether `map` rings a bell. It's `Functor`'s `fmap` from the previous course!
- Ask about parallelization. Is it easy? Why?

---

# Functional toolbox: folding

```bash
> :type foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
# Think of type 't' as list
# Think of the function's second parameter as the accumulator and its
# return value as an intermediate result.
```

<!-- exdown-skip 1 2 7 8 -->
```hs
wordCount :: [[String]] -> Int
wordCount files = undefined

-- | 'Word16' is a zero or positive number
data Operation = Debit Word16 | Credit Word16

balance :: [Operation] -> Word16
balance = undefined
```

???

```hs
wordCount :: [[String]] -> Int
wordCount files = foldr (\words soFar -> (length words) + soFar) 0 files

balance' :: [Operation] -> Word16
balance' = foldr (\op soFar -> toInt op + soFar) 0
  where
    toInt = \case Debit x -> -x; Credit x -> x
```

- Ask who knows about map/reduce

---

# Recap

How to build functions from:

- Pattern matching (`case ... of `)
- Guards (`f x | cond x = ...`)

How to compose functions:

- `(&)`: chain
- `(<&>)`: chain in presence of wrapping
- `Functor`, `map`

Functional toolbox:

- Recursion
- Folding

<!-- - `(>>=)`: chain computations while in a special context -->

---

# Recommended Reading

- http://learnyouahaskell.com/syntax-in-functions
- http://learnyouahaskell.com/higher-order-functions

---

# More composition

```bash
> import Data.Functor
> :type (<&>)
(<&>) :: Functor f => f a -> (a -> b) -> f b
> :type fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

- Given an element of type `a` wrapped in something (`f`), and a function from `a` to `b`, apply the function inside the wrapping.

- What can be `f`?

???

```bash
> import Data.List.Extra
> :type upper
upper :: String -> String
> applyUpper = fmap upper
> applyUpper (Just "foo")
> applyUpper (Right "foo" :: Either Int String)
```

???

If done at this point and time remains:

- Explain `foldr`
- Explain things defined in terms of `foldr`:
  - `sum`
  - `and :: [Bool] -> Bool`
  - `any :: (a -> Bool) -> [a] -> Bool`

---

# More composition

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

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```java
}
```
-->
