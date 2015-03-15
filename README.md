# runstaskell

`runstaskell` is an interpreter for Haskell programs. It's a thin wrapper
around `runhaskell`.  The difference is that `runstaskell` provides a defined
set of packages that you can import from in the executed scripts. So if you
include a line in a script like this:

``` haskell
#!/usr/bin/env runstaskell
```

this says two things:

- This script might import things from the staskell set of packages and
  therefore might not work with vanilla `runhaskell`.
- This script does *not* depend on any packages that are not included in the
  staskell set (or shipped with ghc).

## status

`runstaskell` is not even experimental yet. Don't use it!

## todos

- document how runstaskell uses stackage
- run bootstrapping through cabal install?
- implement proper command line parsing
  - --version
  - --help
  - --list-package-sets
- figure out which packages to include
- figure out stability of stackage lts releases
