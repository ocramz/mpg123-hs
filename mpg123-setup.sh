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
# mkdir temp
# cd temp
# tar -vxzf /mnt/mydownloads/archive.tar.gz
curl -L https://www.mpg123.de/download/${ARCHIVE} > ${ARCHIVE}
tar -vxzf ${ARCHIVE}
# bunzip2 ${ARCHIVE} # mpg123-${VERSION}
# wget -c https://www.mpg123.de/download/${ARCHIVE} -O - | bunzip2 > ${ARCHIVE_DIR}

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


# # # check env
printf "\n=== Environment :\n"
printenv
