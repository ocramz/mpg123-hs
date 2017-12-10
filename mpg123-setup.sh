#!/bin/bash

# https://www.mpg123.de/download/mpg123-1.25.8.tar.bz2

VERSION=1.25.8
ARCHIVE_DIR=mpg123-${VERSION}
ARCHIVE=${ARCHIVE_DIR}.tar.bz2

printf "\n=== APT-Installing dependencies :\n"
apt-get update && apt-get install -y --no-install-recommends build-essential bzip2


printf "\n=== making archive directory\n"
mkdir ${ARCHIVE_DIR}

printf "\n=== downloading mpg123\n"
curl -L https://www.mpg123.de/download/${ARCHIVE} > ${ARCHIVE}
tar -jxf ${ARCHIVE}

pwd; ls -lsA

printf "\n=== entering build dir\n"
cd ${ARCHIVE_DIR}

pwd; ls -lsA

printf "\n=== configuring mpg123\n"
./configure
printf "\n=== building mpg123\n"
make
printf "\n=== installing mpg123\n"
make install


printf "\n=== sudo ldconfig\n"
ldconfig


# # # # check env
# printf "\n=== Environment :\n"
# printenv
