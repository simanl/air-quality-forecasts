#! /bin/bash

wget $@ -O ./tmp/tablas-iniciales.tar.bz2  \
  https://www.dropbox.com/s/u5obj9wbfuqbrzp/tablas-iniciales.tar.bz2?dl=0 \
  && tar xvf ./tmp/tablas-iniciales.tar.bz2 --no-same-owner -C ./tablas

rm -rf tmp/tablas-iniciales.tar.bz2
