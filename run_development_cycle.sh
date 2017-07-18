#!/bin/bash

# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

function build {
  cd src
  ocamlbuild -use-ocamlfind -no-links -plugin-tag "package(js_of_ocaml.ocamlbuild)" -tag debug $@
  cd ..
}

build drawing_tests_in_command_line.byte
rm -f drawing_tests/*/*.png
src/_build/drawing_tests_in_command_line.byte

if ! [ -d node_modules ]
then
    npm install canvas pixelmatch browserify
fi
# https://github.com/mapbox/pixelmatch#install
node_modules/.bin/browserify -s pixelmatch node_modules/pixelmatch/index.js > src/_build/pixelmatch.js

build drawing_tests_in_javascript.js
node drawing_tests_in_node.js

echo
echo "Have a look at $(pwd)/drawing_tests_in_browser.html"
echo

build JsOfOCairo.cma

# OPAM package
# ============

opam pin --yes --no-action add .
opam reinstall --yes JsOfOCairo

cd demo
./demo.sh
