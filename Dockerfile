FROM haskell:7.8

# Add srclib (unprivileged) user
RUN useradd -ms /bin/bash srclib
RUN mkdir -p /src /srclib
RUN chown -R srclib /src /srclib

# Update package lists
RUN cabal update
RUN apt-get update
RUN cabal install cabal-install

# Setup cabal-install for the srclib user.
USER srclib
RUN cabal update
RUN cabal install cabal-install
USER root

# Install C libraries and build tools
RUN apt-get install -y libicu-dev
RUN apt-get install -y build-essential
RUN apt-get install -y autotools-dev dh-autoreconf

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

# Setup srclib-haskell
USER srclib
ENV PATH /home/srclib/.cabal/bin:$PATH
WORKDIR /src
ENTRYPOINT ["srclib-haskell"]
