# [H](https://en.wikipedia.org/wiki/Haskell)enerator

```console
$ docker build . --tag ghc_boiler
$ docker run -it --detach --name henerator -v $(pwd):/home/foo/project ghc_boiler
$ docker exec -it henerator bash
$ cabal init
$ cabal update
$ cabal run
```

The Glorious Glasgow Haskell Compilation System, version 9.8.2
cabal-install version 3.10.3.0
compiled using version 3.10.3.0 of the Cabal library

---

№4 in [programming-challenges](https://github.com/kombarowo/programming-challenges)
