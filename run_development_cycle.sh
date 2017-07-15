#!/bin/bash

# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

function build {

ocamlbuild \
    -use-ocamlfind -no-links \
    -plugin-tag "package(js_of_ocaml.ocamlbuild)" \
    -pkgs js_of_ocaml,js_of_ocaml.ppx,cairo2,General \
    -cflags -w,@a-33-44,-strict-sequence \
    $@
}

build drawing_tests.byte

rm -f drawing_tests/*.png
_build/drawing_tests.byte

build drawing_tests_js.js

echo
echo "Have a look at $(pwd)/drawing_tests.html"
echo

# OPAM package
# ============

opam pin --yes --no-action add .
opam reinstall --yes JsOfOCairo
