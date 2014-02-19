#!/bin/sh -e
ghc-pkg unregister hyperreactive-banana || true
ghc-pkg unregister hyperreactive-sodium || true
(cd hyperreactive && cabal install)
(cd hyperreactive-sodium && cabal install)
(cd hyperreactive-banana && cabal install)
