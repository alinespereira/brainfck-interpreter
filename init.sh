#!/bin/bash

cabal new-update
cabal new-install --only-dependencies
cabal new-configure --enable-tests
cabal new-test
