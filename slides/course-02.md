class: center, middle

# Functional programming in Haskell

## Lesson 2

![Tweag logo](img/tweag.png) ![TN logo](img/tn.png)

<br/>

Clément Hurlin

[https://github.com/smelc/tn-fp-haskell-course]([https://github.com/smelc/tn-fp-haskell-course)

???

Discuss:

- The various roles there can be in the career of a developer:
  - Tech lead
  - Architect
  - Google PGM
  - Engineering Manager

Small companies VS big companies.

In small companies you can:

- participate in business calls, hiring, define the culture
- earn more

In large companies you can:

- earn more

  

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```hs
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Course02 where

import Prelude hiding (Either, filter, Just, Left, Maybe, Nothing, Right, take)
```

```java
class Course02 {
```
-->

---

# Types: Bool

<!-- exdown-skip -->
```hs
data Bool =
    True 
  | False
```

[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) - _read–eval–print loop_

```shell
$ cabal repl
> :type True  # From now on, the > prefix indicates a REPL
True :: Bool
> False || True
True
> :type (||)
(||) :: Bool -> Bool -> Bool
```

- Operators (function with two arguments) are defined enclosed
  with parentheses, but are used without.

```shell
> True /= False
True
```

- The disequality operator is not `!=`, it's `/=`

---

# Types: Int


```shell
> :type 0
0 :: Num p => p
```

Types are inferred; i.e. guessed by the compiler.
The Haskell compiler, `GHC` infers the most general type:

- The type of `0` is any `p` which satisfies some **constraint** `Num`
- TLDR; when the type of an expression is too general, write the expected type:

```shell
> :type (0 :: Int)
type (0 :: Int) :: Int
```

---

# Types: Int

Operators on `Int`:

```shell
> :type (+)
(+) :: Num a => a -> a -> a
```

`+` is polymorphic in its operands: it requires operands to be of
the same type, but the type itself is abstract.

```shell
> 1 + "foo"
• No instance for (Num [Char]) arising from a use of ‘+’
```

--

```shell
> (1 :: Int) + (1.0 :: Float)
• Couldn't match expected type ‘Int’ with actual type ‘Float’
> (1 :: Int) + 1
2                  -- Right '1' was inferred of type Int
> (1 :: Float) + 1
2.0                -- Right '1' was inferred of type Float
```

---

# Types: List

Lists are built from:

- the empty list `[]`
- the concatenating operator `:` 

```shell
> :type [] 
[] :: [a]  # The empty list is polymorphic. In Java terms
           # [] is of type List<T> for all T

> 0 : 1 : []  # List with two elements
[0, 1]
```

⚠️  The `[]` notation is used both in expressions, such as `[0, 1]` and
in types, such as `[Int]` (list of `Int`s). ⚠️

--

Strings are lists of characters:

<!-- exdown-skip -->
```hs
type String = [Char] -- defines a type alias
```

---

# Types: List

<!-- exdown-skip  -->
```hs
-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n >= length xs.
take :: Int -> String -> String
take n xs = undefined
```

--

```hs
take :: Int -> [a] -> [a]
take n _          | n <= 0 = []
take _ []                  = []
take n (x : rest)          = x : take (n - 1) rest
```

???

Note that `n <= 0` is what makes the function total.

--

<!-- exdown-skip  -->
```hs
-- filter, applied to a predicate and a list,
-- returns the list of those elements that satisfy the predicate; i.e.,
-- filter odd [1, 2, 3] == [1, 3]
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = undefined
```

--

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter _ []                     = []
filter p (x : rest) | p x       = x : filter p rest
                    | otherwise = filter p rest
```

---

# Types: Maybe

```hs
data Maybe a = -- Like Optional in Java
    Nothing
  | Just a

-- | Returns the first element of a list, if any
safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x
```

--

- `Maybe Int` is a type
- `Nothing` is an expression, that has type `Maybe a` for any `a`
- `Just` is a function of type `a -> Maybe a`
- `Just "foo"` is an expression of type `Maybe String`

<center>
Data constructors are <b>functions</b>! Constants are parameterless functions.
</center>

--

<center>
<br/>
Function searcher: <a href="hoogle.haskell.org">hoogle.haskell.org</a>
</center>

???

Which other function can we define of type `[a] -> Maybe a`?

```hs
safeLast :: [a] -> Maybe a
safeLast []  = Nothing
safeLast [x] = Just x
safeLast (_ : rest) = safeLast rest
```

---

# Types: Either

```hs
data Either a b =
    -- | The left case, commonly used as the error case
    Left a
    -- | The right case, commonly used as the valid case
  | Right b

-- | Parse a string using the Read instance. Succeeds if there is
-- exactly one valid result. A Left value indicates a parse error.
readEither :: Read a => String -> Either String a
```
<!--
```hs
readEither _ = undefined
```
-->

```shell
> (readEither "0") :: Either String Int
Right 0
> (readEither "not an int") :: Either String Int
Left "Prelude.read: no parse"  # Not the best error message
```

---

# Types: tuples

Tuples differ from lists:

- Tuples have a fixed length
- Elements of tuples do not need to be of the same type

```shell
> :type ("Chris", 42 :: Int)
("Chris", 42 :: Int) :: ([Char], Int)
> :type ("Chris", "Pratt", 42 :: Int)
("Chris", "Pratt", 42 :: Int) :: ([Char], [Char], Int)
> :type fst
fst :: (a, b) -> a
> :type snd
snd :: (a, b) -> b
> fst ("0", "1", "2")
• Couldn't match expected type ‘(a, b0)’
                  with actual type ‘([Char], [Char], [Char])’
```

To deconstruct a tuple of length > 2:

```shell
> get_4 (_, _, _, x) = x
> :type get_4
get_4 :: (a, b, c, d) -> d
```

---

# Types: records

```hs
data Account = Account {
    balance :: Int,
    email :: String,
    name :: Maybe String
  }
```

???

- Distinguish the type (left `Account`) from the value constructor (right `Account`)
- Talk about field names being functions
- Mention `{-# LANGUAGE DuplicateRecordFields #-}`

--

```hs
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

mkAccount :: String -> Account
mkAccount email = Account { balance = 0, email, name = Nothing }

mkAccount2 :: String -> Account
mkAccount2 email = Account { .. } -- Take fields from enclosing scope
  where
    balance = 0
    name = Nothing
```

--

```hs
setBalance :: Int -> Account -> Account
setBalance n account = account { balance = n } -- functional update
```

---

# Types: IO

- `IO a` represents a promise that needs to inspect the environment
  (hence to perform an _input output_ action) to return a value of type `a`
- What is the _environment_?

--

```hs
-- http://learnyouahaskell.com/input-and-output

main :: IO () -- The program entry point, always
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

```shell
> :type putStrLn
putStrLn :: String -> IO ()  # Write something to the terminal
> :type getLine
getLine :: IO String  # Read a line from standard input
```

---

# Types: IO


[//]: #exdown-skip
```hs
main :: IO () -- The program entry point, always
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

```shell
> :type putStrLn
putStrLn :: String -> IO ()
> :type getLine
getLine :: IO String
```

Expressions in a `do` statement of type `IO a`:

- Execute sequentially
- Must be of the form:
  - `x <- someIO`, where `x :: a` and `someIO :: IO a`
  - `y`, where `y :: IO ()`

---

# Types : Tree

- How would you define a tree containing values of type `a`?

Define functions on your trees:

- `values :: Tree a -> [a]`
- `depth :: Tree a -> Int`
- `fmap :: (a -> b) -> Tree a -> Tree b`

---

# Recap

- Literal types: `Bool`, `Int`
- `[a]`, `Maybe a`, `Either a b`
- Tuples and records
- `IO a`

???

- Ask for relations/functions between these types

---

# Recommended Reading

- http://book.realworldhaskell.org/read/types-and-functions.html
- https://www.seas.upenn.edu/~cis194/spring13/lectures/02-ADTs.html
- https://www.seas.upenn.edu/~cis194/spring13/lectures/03-rec-poly.html

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```java
}
```
-->
