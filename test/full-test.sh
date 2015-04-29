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

runstaskell --bootstrap custom-1

runstaskell --list-bootstrapped

runstaskell-custom-1 test/03.hs

runstaskell-custom-1 test/04.hs
