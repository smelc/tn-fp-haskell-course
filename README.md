## Course on functional programming - Haskell

This course is given at [Telecom Nancy](https://smelc.github.io/tn-fp-haskell-course/slides/)
at the end of 2021 in _3A_.

### Online version

Visit [https://smelc.github.io/tn-fp-haskell-course/slides/](https://smelc.github.io/tn-fp-haskell-course/slides/)

### Offline version, for writing the slides

The [slides](https://github.com/smelc/tn-fp-haskell-course/blob/master/slides)
folder uses [remark](https://github.com/gnab/remark).
To display the slides (be it for presenting or for developing them),
you need to serve the `slides` directory with an http server:

```shell
python3 -m http.server  # in slides/
# Or use ./run.sh
```

Now, crawl `localhost:8000` and open the various `html` files.

Note that it's possible to develop the slides offline,
as per the instruction on the
[remark wiki](https://github.com/gnab/remark/wiki#offline-use-without-an-internet-connection).
But I've never done it.
