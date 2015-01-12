FROM haskell:7.8

# Add srclib (unprivileged) user
RUN useradd -ms /bin/bash srclib
RUN mkdir -p /src /srclib
RUN chown -R srclib /src /srclib

# This will cache `cabal update` to keep code updates from re-installing.
RUN cabal update

# Add the source code and compile.
ADD ./haddock /srclib/srclib-haskell/haddock
WORKDIR /srclib/srclib-haskell/haddock
RUN cabal install --global -j4 --disable-optimization

ADD ./srclib-haskell.cabal /srclib/srclib-haskell/srclib-haskell.cabal
WORKDIR /srclib/srclib-haskell
RUN cabal install --global -j4 --only-dependencies

ADD ./src /srclib/srclib-haskell/src
ADD ./LICENSE /srclib/srclib-haskell/LICENSE
WORKDIR /srclib/srclib-haskell
RUN cabal install --global --disable-optimization -j4

# Make sure our fork of haddock is choosen while graphing.
ENV PATH /usr/local/bin/:$PATH

# Run
WORKDIR /src
ENTRYPOINT ["srclib-haskell"]
