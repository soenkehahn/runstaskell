#!/usr/bin/env bash

set -o errexit

rm -rf test-install-prefix

cabal install --prefix $(pwd)/test-install-prefix

export PATH=./test-install-prefix/bin:$PATH

runstaskell --list-installable

runstaskell --bootstrap test

runstaskell-test --list-bootstrapped

runstaskell-test test/01.hs
runstaskell-test test/02.hs

runstaskell --bootstrap rc-2.5

runstaskell --list-bootstrapped

runstaskell-rc-2.5 test/03.hs
runstaskell test/03.hs

runstaskell-rc-2.5 test/04.hs
