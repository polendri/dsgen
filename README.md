dsgen
=====

A utility for generating randomized kingdom card sets for the board game Dominion.

Install instructions:

sudo apt-get install alex cabal-install ghc happy libgtk-3-dev
cabal update
cabal install gtk2hs-buildtools network
export PATH=$HOME/.cabal/bin:$PATH
./build-linux

