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

Usually you probably want to fix the major part of the version of LTS, but not the minor part, e.g.:
``` haskell
#!/usr/bin/env runstaskell-1
```
See https://github.com/fpco/lts-haskell#readme for more information on the stability of LTS.

## status

`runstaskell` is not even experimental yet. Don't use it!

## todos

- make `runstaskell-rc-1` work.
- put list of packages in a file
- figure out which packages to include
- document how runstaskell uses stackage
- implement partial versions (runstaskell-1)
- run bootstrapping through cabal install (through the Setup.hs script)?
- implement proper command line parsing
  - --version
  - --help
  - --list-package-sets
