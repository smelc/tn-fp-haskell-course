class: center, middle

# Functional programming in Haskell

## Lesson 4

![Tweag logo](img/tweag.png) ![Modus logo](img/modus-create.png)

<br/>

![TN logo](img/tn.png)

Clément Hurlin

[https://github.com/smelc/tn-fp-haskell-course]([https://github.com/smelc/tn-fp-haskell-course)

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```hs
{-# Language DeriveFunctor #-}
{-# Language MultiParamTypeClasses #-}

module Course04 where

import Data.Word
import Prelude hiding ((==), Bounded, Enum, Eq, Ordering, Show)
```

```java
class Course04 {
```
-->

---

# Previously

* How to define generic types: `Maybe a`, `[a]`
* How to define concrete functions: `Maybe String -> String -> String`

In this course:

<center>
  Generic functions
</center>

<br/>

Genericity in Haskell is twofold:

* **Polymorphism**: functions that work for generic types
* **Classes constraints**: require parameters to have some interface

???

* Ask for functions that work for generic types:
  * List functions: `filter`, `take`, etc.
  * Map functions: `elem`
  * Other containers: `fromMaybe`, `bimap`

* Ask for interfaces in `Java`:
  * `equals`, `hashCode`, `clone` are interfaces; which are maybe
    supported, maybe not
  * `List`, `Map`

---

# Typeclasses: `Eq`

<center>
⚠️ The Haskell <code>class</code> keyword  is unrelated to object-oriented classes 💣
</center>

```hs
-- | Types whose values can be compared.
-- Expected to have the following properties:
-- Reflexivity: @x == x@ is @True@
-- Symmetry:    @x == y@ is @y == x@
-- etc.
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  (/=) a b = not (a == b)  -- Default implementation
```

--

Define:

<!-- exdown-skip  -->
```hs
instance Eq Bool where
  (==) a b = undefined
```

--

<!-- exdown-skip 4 5 -->
```hs
instance Eq a => Eq (Maybe a) where
  (==) _a _b = undefined

instance Eq a => Eq [a] where
  (==) _xs _ys = undefined
```

???

- Discuss `equals` and `hashCode` in Java

```hs
instance Eq a => Eq [a] where -- Build bigger instances from smaller ones
  (==) [] []                         = True
  (==) (a : as) (b : bs) | a == b    = as == bs
  (==) _ _               | otherwise = False
```

---

# More base typeclasses

```hs
data Ordering = LT | EQ | GT

-- | Totally ordered datatypes
class Ord a where
  compare :: a -> a -> Ordering
  (<=) :: a -> a -> Bool
  -- Other operators: <, >, >=, min, max
```

???

* Ask for the Java equivalent of `compare`

--

```hs
-- | Types that can be pretty printed
class Show a where
  show :: a -> String
```

--

```hs
-- | Types whose values can be enumerated
class Enum a where
  succ :: a -> a
  pred :: a -> a
  -- Other functions

class Bounded a where
  minBound :: a
  maxBound :: a
```

???

* Ask for types that are instances of `Enum` and `Bounded`:
  - `Int` and `Bool`

---

# Be lazy, rely on the compiler

<center>
<b>deriving</b>
</center>

<!-- exdown-skip 6 -->
```hs
data Version =
    Alpha
  | Beta
    -- | Version number of the form "x.y.z"
  | SemVer Int Int Int
  deriving (Eq, Show)
```

* Makes the compiler generate implementation for `Eq Version` and
  `Show Version` 💪
* Java 15 (2020) has this for _record_ classes
* Works for a number of classes: `Enum`, `Bounded`, `Functor`, `Writer`, etc.

???

- Mention that IDEs do that
- Ask them what editors they use and why

---

# Typeclasses vs Java interfaces

Typeclasses bear similarities with Java interfaces, but:

- When a Java class is defined, all its interfaces must be declared. Typeclasses,
  however, don't have to be defined with the type.
- Typeclasses are more general because they support multiple parameters:

```hs
-- | Values that are initialized from a single parameter.
class Empty a b where
  empty :: a -> b
```

---

# Typeclasses vs Java interfaces

In addition, because of dynamic dispatch and subtyping, compare:

[//]: #exdown-skip
```hs
class Num a where
  (+) :: a -> a -> a
```

and

```java
interface Addable<A> {

  public A add(A right); // 'this' is the left operand
  
}
```

---

# Typeclasses vs Java interfaces

Since there is no subtyping (and no concept of _trait object_ or
_interface object_), we can't build nor work on heterogeneous collections.
Compare:

[//]: #exdown-skip
```hs
-- | Can sum a list of @a@s when @a@ has the 'Num' capability.
-- But all the items in the list must have the concrete type @a@,
-- and the returned value will have the same type @a@.
sumList :: Num a => [a] -> a
```

[//]: #exdown-skip
```java
interface Num { ... }
class NInt implements Num { ... }
class NDouble implements Num { ... }
/** 'nums' can contain some items of concrete type NInt,
  * and some items of concrete type NDouble at the same time.
  * The returned value can be either a NInt or NDouble. */
Num sumList(nums: List<Num>) { }
```

---

# Typeclasses: `Semigroup`

[//]: #exdown-skip
```hs
-- | A semigroup is a type whose values can be reduced or aggregated
-- Instances should satisfy @x <> (y <> z) = (x <> y) <> z@
class Semigroup a where
  (<>) :: a -> a -> a
```

```hs
data BankAction =
    -- | Une dépense
    Spend Int
    -- | Un gain
  | Earn  Int
    -- | Compte cloturé
  | Close
```

--

```hs
instance Semigroup BankAction where
  Spend i <> Spend j = Spend (i + j)
  Spend i <> Earn j  = Spend (i - j)
  Close   <> _       = Close
  _       <> Close   = Close
  Earn i  <> Spend j = Earn (i - j)
  Earn i  <> Earn j  = Earn (i + j)
```

???

- Ask the version with `Nat` in place of `Int`

--

* `instance Semigroup [a]`
* `instance Semigroup a => Semigroup (Maybe a)`

---

# Typeclasses: `Monoid`

[//]: #exdown-skip
```hs
class Semigroup a => Monoid a where
  -- | The neutral element
  mempty :: a
  -- | Merging operation
  mappend :: a -> a -> a
  -- | Reduction, has a default implementation
  mconcat :: [a] -> a
```

???

- Show the laws on `Hoogle`
- Ask for `mempty` and `mappend` on lists

--

```hs
-- Only >= 0 number of votes makes sense
type Nat = Word16

data Reactions = Reactions {
    hearts :: Nat,
    thumbsUp :: Nat,
    thumbsDown :: Nat
  }
```

???

- Ask to write `mempty`
- Ask to write `mappend`
- Implement `instance Monoid (a, b)`

[//]: #exdown-skip
```hs
instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)
  (a,b) `mappend` (c,d) = (a `mappend` c, b `mappend` d)
```

- Ask for another monoid implementation (hits in search of multiple files)

---

# Typeclasses: `Functor`

[//]: #exdown-skip
```hs
class Functor f where
  -- | Apply a function to an element wrapped in something ('f')
  fmap :: (a -> b) -> f a -> f b
```

- What does `fmap` do?
- Give instances:
  - `Maybe a`
  - `[a]`

---

# Be lazy, rely on the compiler

From course 2:

```hs
data Interval a = Interval {
    start :: a,
    end :: a
  }
  deriving Functor
```

- Write `Functor Interval`

---

# Typeclasses for `QuickCheck` testing

`QuickCheck` is a library for testing properties of functions.

[//]: #exdown-skip
```hs
-- | Random generation and shrinking of values.
class Arbitrary a where
  -- | A generator of 'a'
  arbitrary :: Gen a
  -- | Given an 'a', smaller versions of 'a'
  shrink :: a -> [a]

-- | Generates one of the given values
elements :: [a] -> Gen a

-- | Chooses one of the given generators, with a weighted random distribution.
frequency :: [(Int, Gen a)] -> Gen a
```

---

# Recap

Typeclasses:

- Choose runtime behavior based on static types (/= dynamic dispatching)
- Leverage the compiler's generation capabilities
- Build APIs and features bottom up:
  - from small values, to more structured values.

---

# Recommended reading

- http://learnyouahaskell.com/types-and-typeclasses#typeclasses-101
- http://book.realworldhaskell.org/read/using-typeclasses.html
- https://wiki.haskell.org/Typeclassopedia

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```java
}
```
-->
