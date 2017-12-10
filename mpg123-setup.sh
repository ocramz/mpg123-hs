#!/bin/bash

# https://www.mpg123.de/download/mpg123-1.25.8.tar.bz2

VERSION=1.25.8
ARCHIVE=mpg123-${VERSION}.tar.bz2

printf "\n=== APT-Installing dependencies :\n"
apt-get update && apt-get install -y --no-install-recommends build-essential bzip2




curl -L https://www.mpg123.de/download/${ARCHIVE}
bunzip2 ${ARCHIVE}

./configure
make
make install


# # # check env
printf "\n=== Environment :\n"
printenv
