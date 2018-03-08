#!/bin/bash

# Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

eval `opam config env`
opam install --yes js_of_ocaml-ppx js_of_ocaml-compiler cairo2 General jbuilder bisect_ppx bisect-summary

if ! [ -d node_modules ]
then
    npm install canvas pixelmatch browserify
fi
# https://github.com/mapbox/pixelmatch#install
node_modules/.bin/browserify -s pixelmatch node_modules/pixelmatch/index.js > tst/pixelmatch.js

clear

rm -f _build/default/tst/*.sentinel

# https://github.com/aantron/bisect_ppx/blob/master/doc/advanced.md#Jbuilder suggests
# modifying the jbuild file for release. Let's modify it for tests instead.
sed -i "s/^;\(.*bisect_ppx).*\)$/\1/" $(find . -name jbuild)
jbuilder runtest --dev
sed -i "s/^\(.*bisect_ppx).*\)$/;\1/" $(find . -name jbuild)
echo
bisect-summary _build/default/tst/bisect????.out
echo
bisect-ppx-report -I _build/default -html _build/default/bisect _build/default/tst/bisect????.out
echo "See coverage report in $(pwd)/_build/default/bisect/index.html"

echo
echo "Check test results in $(pwd)/_build/default/tst/tests_in_browser.html"
echo

rm -f docs/*.html docs/*.png docs/*.txt docs/*.js
cp _build/default/tst/Tests/Drawing/Cairo/*.png docs
cp _build/default/tst/Tests/Limitations/*.png docs
cp _build/default/tst/Tests/Limitations/*.txt docs
cp _build/default/tst/tests_in_browser.html docs/index.html
sed "s|Tests/Drawing/Cairo/||g; s|Tests/Limitations/||g" _build/default/tst/tests_in_browser.bc.js > docs/tests_in_browser.bc.js
cp _build/default/tst/pixelmatch.js docs

# OPAM package
# ============

opam pin --yes --no-action add .
opam reinstall --yes JsOfOCairo

cd demo
./demo.sh

echo
echo "Development cycle OK"
