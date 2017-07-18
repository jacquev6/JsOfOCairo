#!/bin/bash
# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

ocamlbuild \
  -use-ocamlfind -no-links -plugin-tag "package(js_of_ocaml.ocamlbuild)" \
  draw_in_browser.js draw_on_command_line.byte

_build/draw_on_command_line.byte

echo
echo "Have a look at $(pwd)/draw_in_browser.html"
