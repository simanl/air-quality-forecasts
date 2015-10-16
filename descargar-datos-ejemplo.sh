#! /bin/bash

wget -O ./tmp/sample-data-csvs.tar.bz2 \
  "https://www.dropbox.com/s/39d36j27a0dgssu/sample-data-csvs.tar.bz2?dl=0" \
  && tar xvf ./tmp/sample-data-csvs.tar.bz2 --no-same-owner -C ./tmp

rm -rf ./tmp/sample-data-csvs.tar.bz2
