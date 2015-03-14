#!/usr/bin/env bash

set -o errexit

rm -rf test-install-prefix

cabal install --prefix $(pwd)/test-install-prefix

export PATH=./test-install-prefix/bin:$PATH

staskell-bootstrap test

runstaskell-test --list

runstaskell-test test/01.hs
runstaskell-test test/02.hs

staskell-bootstrap 1.11

runstaskell --list

runstaskell-1.11 test/03.hs
runstaskell test/03.hs
