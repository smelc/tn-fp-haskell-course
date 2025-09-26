class: center, middle

# Functional programming in Haskell

## Lesson 1

<img src="img/tweag_logo_black.png" height="64">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;![TN logo](img/tn.png)

<br/>

Clément Hurlin, Director of Engineering, Tweag

[https://github.com/smelc/tn-fp-haskell-course]([https://github.com/smelc/tn-fp-haskell-course)

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```hs
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Course01 where

import Prelude hiding (reverse)
```

```java
import java.util.*;
import java.util.stream.*;

class Course01 {
```
-->

---

# Why functional programming?

Functional languages:

- Scala
- Haskell
- OCaml

Why use them:

- Used in financial institutions: _earn more money_
  - BNP Paribas - Haskell
  - [Standard Chartered](https://www.sc.com/) - Haskell
  - [Tezos](https://tezos.com/) - OCaml
  - Trading companies:
     - [Morgan Stanley](https://www.morganstanley.com/)
     - [Jane Street](https://www.janestreet.com/) - OCaml
- _Work on interesting projects_:
  - European Space Agency 🚀
  - Tesla - Haskell
  - [Trip Shot](https://www.tripshot.com/) - Haskell

???

- Discuss incomes after graduating from TN
- Salaire débutant ESN: 35K-40K

---

# Why functional programming?

💯 My personal recommendations 😍

- [Cardano](https://cardano.org/), Haskell: [https://apply.workable.com/io-global](https://apply.workable.com/io-global)
- [Mercury](https://mercury.com/), Haskell: [https://mercury.com/jobs](https://mercury.com/jobs)
- [Swan](https://fr.swan.io/), Scala: [https://www.welcometothejungle.com/fr/companies/swan/jobs](https://www.welcometothejungle.com/fr/companies/swan/jobs)
- [Flowie](https://www.get-flowie.com/) - Scala

</br>
</br>

<center>
Those teams are at the top of their craft 💪
</center>
</br>
<center>
You will learn a lot working there
</center>

???

- Explain why there are many banking systems in here:
  - GC okay
  - Care about correctness

---

# Why functional programming?

At its heart, functional programming is simpler than imperative programming:

- No pointers
- No subtyping
- No mutations

```java
/* Java 7 */
List<Integer> evens(List<Integer> xs) {
  final List<Integer> result = new ArrayList<Integer>(xs.size() / 2);
  for (Integer x : xs) {
    if (x % 2 == 0)
      result.add(x);
  }
  return result;
}
```

```hs
-- Haskell
evens []                        = []
evens (x : rest) | x mod 2 == 0 = x : evens rest
                 | otherwise    = evens rest
```

???

- Ask for basic blocks of Java programs:
  - Primitive values (`int`)
  - Objects

Highlight how things are more complicated in Java:

- Need to handle `null`
- Alternative implementations possible: in place change
- Should the result be immutable?

- Discuss different kind of lists

---

# Course objective

This course does NOT intend to make you think functional programming
is better. The point is to expand your mind:

- Imperative programming is about **state**
- Functional programming is about **values**

???

No "language fud"

--
<br/>


- There is a lot to learn by thinking in terms of values
- Passing values around rather than mutating state makes for safer programs,
  and programs that are easier to test 👍

<center>
<br/>
Knowing functional programming will make you a better programmer,
no matter your daily language.
</center>

???

When dealing with state, you need to have context in mind:

- Context of external databases
- Context of disk
- Network, etc.

---

# Functional programming spreads in imperative languages

Java ☕

- 2014: Java 8 brings _streams_, _lambdas_, _Optional_, etc.
- 2018: Java 10 brings local variable type inference
- 2020: Java 15 brings records and sealed classes, and more pattern matching
- 2019 - 2025: Java 12, 16, 17, 19, 20, and 21 improve pattern matching

Python 🐍

<!-- - 1994: version 1.0 has `lambda`, `map`, `filter` -->
- 2000: version 2.0 brings list comprehension
<!-- - 2001: version 2.7 brings `dict` comprehension -->
- 2015: version 3.5 brings types hinting
- 2020: version 3.10 brings pattern matching
<!-- - 2022: version 3.11 brings more types -->
- 2023: version 3.12 improves ergonomics for type parameters syntax and generic types
- 2024: version 3.13 brings readonly-type for `dict` and type narrowing

The same goes for: C#, Javascript, C++

???

- Do you know what `Optional` is?
  - `lambdas`?
  - `streams`?

---

# Why Haskell?

- There's no escape from functional programming in Haskell (`/=` OCaml)
- It's really good for _type safety_
  - Type safety: rely on the compiler to verify a lot of things
  - Concise to define new types: encourages type safety

It's really good for testing:

- Easy to mock data (use fake implementations: fake database)
- Easy to generate random data: `Generic`

It is a language used both by:

- Researchers, so the language has a lot of bleeding-edge features
- Industry, so the language is battle-tested for production applications

<center>
<img alt="Haskell logo" src="img/haskell.png">
</center>

???

- Discuss what mocking is
- Discuss what is a CI. Why it is important it goes fast
  - Feedback loops

---

# Language constructs

Instead of loops, functional programs use:

- Recursion
- `map`: mapping
- `foldr`, `foldl`: folding

```java
<T> List<T> reverse(List<T> xs) {
  final LinkedList<T> result = new LinkedList<T>();
  for (T x : xs) {
    result.addFirst(x);
  }
  return result;
}
```

<center>
⚠️ The Haskell <code>-></code> syntax is unrelated to Java lambdas 💣
</center>

```hs
-- | The :: notation reads "has type". It is the equivalent of a Java signature.
-- In Java it would be 'static public List<A> reverse(List<A> elems)'
reverse :: [a] -> [a]
reverse [] = []
reverse (x : rest) = (reverse rest) ++ [x] -- Call reverse within reverse
```

???

- Discuss whether they know map/reduce
- Discuss heap problem/tail call optimization

---

# Language constructs

Exemplifying _map_

```java
List<Integer> toLengths(List<String> xs) {
  final List<Integer> result = new ArrayList<Integer>(xs.size());
  for (String x : xs) {
    result.add(x.length());
  }
  return result;
}
```

```hs
-- map has type :: (a -> b) -> [a] -> [b]
-- in Java it would be 'static public List<B> (Function<A, B> f, List<A> elems)'
-- length has type :: [a] -> Int

toLengths :: [String] -> [Int] -- Type declaration
toLengths xs = map length xs

toLengths2 :: [String] -> [Int]
toLengths2 = map length -- Alternative implementation, partial application
```

```java
List<Integer> toLengthsJava8(List<String> xs) {
  return xs.stream().map(String::length).collect(Collectors.toList());
}
```

---

# Language constructs

In imperative programs, the **sequence** (`;`) is the basic block:

<!-- exdown-skip -->
```java
do_something;
then_do_something_else
```

In functional programs, **expressions** are the basic blocks:

<!-- exdown-skip -->
```hs
mult3 :: Int -> Int -> Int -> Int
mult3 x y z = result
  where -- define variables global to the function, order doesn't matter!
    result = xy * z
    xy = x * y
```

---

# Algebraic Data Types

```hs
data Version =
    Alpha
  | Beta
    -- | Version number of the form "x.y.z"
  | SemVer Int Int Int

data Driver =
    -- | Car is being driven by a human with a name
    Human String
    -- | Car is on autopilot
    | Autopilot Version
```

--

```java
/* Requires Java 15 for sealed classes and interfaces */
public sealed interface Version permits Alpha, Beta, SemVer { }

final class Alpha implements Version { }
final class Beta  implements Version { }

/* Requires Java 14 for records */
record SemVer(int x, int y, int z) implements Version { }
```

???

- Also called _sum_ types

---

# Abstracting: typeclasses

<center>
⚠️ The Haskell <code>class</code> keyword  is unrelated to object-oriented classes 💣
</center>

```hs
-- | Generic class of containers
class Contains a b where
  get :: a -> b

-- | 'MkPerson "Clément" 39'
data Person = MkPerson String Int

-- | A username like GitHub's @smelc
data Username = Account String

instance Contains Person String where
  get (MkPerson firstName _) = firstName

instance Contains Username String where
  get (Account handle) = "@" ++ handle -- Show GitHub's '@'

toString :: Contains a String => a -> String
toString (whatever :: a) = get whatever
```

???

- Explain typeclasses are open abstractions
  - You can define them far away from the type's definition
- Ask what's the problem?
  - Conflicting definitions are possible

<!--
-- | Generic setter for containers
class With a b where
  with :: b -> a -> a

instance With Person String where
  with firstName (Name _ age) = Name firstName age
-->

---

# Mathematical properties and testing

- Well-designed types and APIs enjoy good properties relating their functions
- Boolean, lists, sets
- Arrays: `get`, `set`
- Maps: `get`, `set`
- Orderings

Property-based testing:

- Generate random data
- Check that properties hold using this data
- Super general 💪, reliable 🧱

???

- Discuss the difference between arrays and lists
  - Discuss the temporal properties difference
  - Discuss the importance of "culture générale" during interviews

---

# Recap

- **Expressions** are the basic blocks
- Types are expressive
- Makes testing easier
- Makes decoupling easier (inversion of dependencies)

---

# Recommended Reading

- https://learnyouahaskell.github.io/chapters.html
- http://book.realworldhaskell.org/read/getting-started.html

<!-- Machinery for making the snippets valid, not shown, only
     used by exdown (see check.sh).

```java
}
```
-->
