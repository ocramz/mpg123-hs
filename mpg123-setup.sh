#!/bin/bash

# https://www.mpg123.de/download/mpg123-1.25.8.tar.bz2

VERSION=1.25.8
ARCHIVE_DIR=mpg123-${VERSION}
ARCHIVE=${ARCHIVE_DIR}.tar.bz2

printf "\n=== APT-Installing dependencies :\n"
apt-get update && apt-get install -y --no-install-recommends build-essential bzip2



mkdir ${ARCHIVE_DIR}
# curl -L https://www.mpg123.de/download/${ARCHIVE} | bunzip2 > mpg123-${VERSION}
wget -c https://www.mpg123.de/download/${ARCHIVE} -O - | bunzip2 > ${ARCHIVE_DIR}
cd ${ARCHIVE_DIR}

./configure
make
make install


# # # check env
printf "\n=== Environment :\n"
printenv
