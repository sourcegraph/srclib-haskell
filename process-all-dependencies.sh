#!/bin/sh
cabal-db vcs warp |
  grep '://' | sed 's/^ *//' |
  sed 's|/@|@|' |
  uniq |
  sed '
    s|^[^F].*git://\(.*\)/\([^/@]*\)\.git@\(.*\)$|F\t\3\t\2\t\1/\2.git|;
    s|^[^F].*git://\(.*\)/\([^/@]*\)@\(.*\)$|F\t\3\t\2\t\1/\2.git|;
    s|^[^F].*http://\(.*\)/\([^/@]*\)@\(.*\)$|F\t\3\t\2\thttp://\1/\2|;
    s|^[^F].*https://\(.*\)/\([^/@]*\)@\(.*\)$|F\t\3\t\2\thttps://\1/\2|
    ' |
  sed 's|F\t\(.*\)\t\(.*\)\t\(.*\)$| \
# \2                                 \
set -x;                              \
mkdir -p ~/warpdeps;                 \
cd ~/warpdeps;                       \
git clone "\3";                      \
cd "\2";                             \
git checkout "\1";                   \
rm -r ./.srclib-cache;               \
src do-all -m program;               \
src push                             |
' | sh 2>&1 | tee outfile
