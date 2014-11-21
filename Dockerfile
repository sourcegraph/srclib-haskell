FROM haskell:7.8

# Add srclib (unprivileged) user
RUN useradd -ms /bin/bash srclib
RUN mkdir -p /src /srclib
RUN chown -R srclib /src /srclib

# This will cache dependency resolution to keep code updates from re-installing.
RUN cabal update
ADD ./srclib-haskell.cabal /srclib/srclib-haskell/srclib-haskell.cabal
WORKDIR /srclib/srclib-haskell
RUN cabal install --only-dependencies -j4

# Add the source code and compile.
ADD ./src /srclib/srclib-haskell/src
ADD ./LICENSE /srclib/srclib-haskell/LICENSE
WORKDIR /srclib/srclib-haskell
RUN cabal install
ENV PATH /srclib/srclib-haskell/.bin:$PATH

# Copy the executeable to ./.bin/srclib-haskell
RUN cd /srclib/srclib-haskell; ln -s ./dist/build/srclib-haskell .bin

# Run
USER srclib
WORKDIR /src
ENTRYPOINT ["srclib-haskell"]
