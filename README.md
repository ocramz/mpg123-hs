# mpg123

[![Build Status](https://travis-ci.org/ocramz/mpg123-hs.png)](https://travis-ci.org/ocramz/mpg123-hs) [![Hackage version](https://img.shields.io/hackage/v/mpg123.svg?label=Hackage)](https://hackage.haskell.org/package/mpg123) [![Stackage version](https://www.stackage.org/package/mpg123/badge/lts?label=Stackage)](https://www.stackage.org/package/mpg123)

Haskell bindings to the MP3 decoding library [`libmpg123`](https://mpg123.de)


## Features

- Based on `inline-c`


## Installation

This library relies on dyamically linking to `libmpg123`; the basic installation of `libmpg123`, detailed in the following, is usually sufficient for ensuring this.

* If you don't have `libmpg123` installed already, download it via its [homepage](https://www.mpg123.de/download.shtml) (which will point you to the Sourceforge repository). After you download and decompress the archive, installation follows the usual Unix sequence:

        $ ./configure

        $ make

        $ make install

You might need to be root or use `sudo` for the last step, which copies object files and binaries to default locations. You may choose custom locations for these during the `./configure` step, but you must ensure they are on `$PATH`.

* Now you can compile the Haskell bindings:

        $ stack build

* Optionally, you can run the tests with

        $ stack test

or you can just see the outcome of the automated tests on TravisCI : https://travis-ci.org/ocramz/mpg123-hs/builds 