## Slides

The content is within `*.md` files.

* `index.md` is the index page of the [online version](https://smelc.github.io/tn-fp-haskell-course/)

To check that code snippets are valid, run the following from the repo's top-level:

```shell
cd slides && ./extract_hs.sh; cd -
cabal build all
```

and for Java files:

```shell
cd slides && ./build_java.sh; cd -
```

# Inspiration

This course has not been written from scratch. It takes inspiration from:

- http://learnyouahaskell.com/chapters
- Brent Yorgey's course at Penn University
  - https://www.seas.upenn.edu/~cis194/spring13/lectures.html
- OCaml course at Université Paris Diderot
  - https://gaufre.informatique.univ-paris-diderot.fr/letouzey/pf5/tree/master/
