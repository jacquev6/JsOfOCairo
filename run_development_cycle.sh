#!/bin/bash

# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

ocamlbuild \
    -use-ocamlfind -no-links \
    -plugin-tag "package(js_of_ocaml.ocamlbuild)" \
    -pkgs js_of_ocaml,js_of_ocaml.ppx,cairo2,General \
    -cflags -w,@a-33-44,-strict-sequence \
    drawing_tests.byte drawing_tests_js.js

rm -f drawing_tests/*.png
_build/drawing_tests.byte

echo "Have a look at $(pwd)/drawing_tests.html"
