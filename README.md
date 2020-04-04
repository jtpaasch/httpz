# `httpz`

Some examples of using the `Req` library to make HTTP requests with Haskell.

The `App.Client` module is a wrapper around the `Network.HTTP.Req` package,
and it provides the basic utilities for making requests. 

There are examples of using that library to actually make requests
in the `App.Application` module.

To run the different examples, uncomment the different bits in `Main.hs`.


## Build/run

To build the code (use `cabal new-*` commands for `cabal` less than 3.0):

    cabal build

Run it:

    cabal run httpz

Clean it:

    cabal clean

Load in GHCi:

    cabal repl httpz


