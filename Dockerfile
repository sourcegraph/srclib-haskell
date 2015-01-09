FROM haskell:7.8

# Add srclib (unprivileged) user
RUN useradd -ms /bin/bash srclib
RUN mkdir -p /src /srclib
RUN chown -R srclib /src /srclib

# This will cache dependency resolution to keep code updates from re-installing.
RUN cabal update
ADD ./srclib-haskell.cabal /srclib/srclib-haskell/srclib-haskell.cabal
WORKDIR /srclib/srclib-haskell

# TODO The dependency on our custom haddock breaks this.
# RUN cabal install --only-dependencies -j4

# Add the source code and compile.
ADD ./src /srclib/srclib-haskell/src
ADD ./haddock /srclib/srclib-haskell/haddock
ADD ./LICENSE /srclib/srclib-haskell/LICENSE
WORKDIR /srclib/srclib-haskell/haddock
RUN cabal install -j4
WORKDIR /srclib/srclib-haskell
RUN cabal install --only-dependencies -j4
RUN cabal install

# Make sure that our srclib-haskell and our haddock fork are in the path.
RUN cp ~/.cabal/bin/srclib-haskell ~/.cabal/bin/haddock /usr/bin
ENV PATH /usr/bin:$PATH

# Run
USER srclib
WORKDIR /src
ENTRYPOINT ["srclib-haskell"]
