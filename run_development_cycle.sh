#!/bin/bash

# Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

eval `opam config env`
opam install --yes js_of_ocaml-ppx js_of_ocaml-compiler cairo2 General jbuilder bisect_ppx bisect-summary

if ! [ -d node_modules ]
then
    npm install canvas pixelmatch browserify
fi

clear

# https://github.com/aantron/bisect_ppx/blob/master/doc/advanced.md#Jbuilder suggests
# modifying the jbuild file for release. Let's modify it for tests instead.
sed -i "s/^;\(.*bisect_ppx.*\)$/\1/" $(find . -name jbuild)
jbuilder runtest --dev
sed -i "s/^\(.*bisect_ppx.*\)$/;\1/" $(find . -name jbuild)
if [ -f _build/default/tst/bisect0001.out ]
then
  echo
  bisect-summary _build/default/tst/bisect????.out
  echo
  bisect-ppx-report -I _build/default -html _build/bisect _build/default/tst/bisect????.out
  echo "See coverage report in $(pwd)/_build/bisect/index.html"
else
  echo "Coverage report from previous test run: $(pwd)/_build/bisect/index.html"
fi

# https://github.com/mapbox/pixelmatch#install
node_modules/.bin/browserify -s pixelmatch node_modules/pixelmatch/index.js > _build/default/pixelmatch.js
echo
echo "Have a look at $(pwd)/drawing_tests_in_browser.html"
echo

# OPAM package
# ============

opam pin --yes --no-action add JsOfOCairo .
opam pin --yes --no-action add CairoMock .
opam reinstall --yes JsOfOCairo CairoMock

cd demo
./demo.sh

echo
echo "Development cycle OK"
