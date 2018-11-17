#!/usr/bin/env python3

# Copyright 2018 Vincent Jacques <vincent@vincent-jacques.net>

import sys


def gen(flavor):
    yield '; Files produced by cppo have file-line directives like:'
    yield ';     # 1 "CairoMock.cppo.ml"'
    yield '; They generate errors when dune calls bisect_ppx:'
    yield ';     (cd _build/default && ./.ppx/bisect_ppx/ppx.exe --dump-ast --cookie \'library-name="CairoMock"\' -o src/CairoMock.pp.ml --impl src/CairoMock.ml)'
    yield ';     File "src/CairoMock.ml", line 1:'
    yield ';     Error: I/O error: CairoMock.cppo.ml: No such file or directory'
    yield '; So we use sed to prepend the directory name to the file name, look like:'
    yield ';     # 1 "src/CairoMock.cppo.ml"'
    yield ''
    yield '(rule'
    yield '  (targets CairoMock.mli)'
    yield '  (deps (:src CairoMock.cppo.mli) S.incl.mli)'
    yield '  (action (run %{bin:cppo} -V OCAML:%{ocaml_version} %{src} -o %{targets}))'
    yield ')'
    yield ''
    yield '(rule'
    yield '  (targets CairoMock.intermediate.ml)'
    yield '  (deps (:src CairoMock.cppo.ml) Backend.incl.ml S.incl.mli)'
    yield '  (action (run %{bin:cppo} -V OCAML:%{ocaml_version} %{src} -o %{targets}))'
    yield ')'
    yield '(rule'
    yield '  (targets CairoMock.ml)'
    yield '  (deps (:interm CairoMock.intermediate.ml))'
    yield '  (action (with-stdout-to %{targets} (run %{bin:sed} "s|# \\\\([0-9]\\\\+\\\\) \\"|# \\\\1 \\"src/|" %{interm})))'
    yield ')'
    yield ''
    yield '(rule'
    yield '  (targets JsOfOCairo.intermediate.ml)'
    yield '  (deps (:src JsOfOCairo.cppo.ml) Backend.incl.ml)'
    yield '  (action (run %{bin:cppo} -V OCAML:%{ocaml_version} %{src} -o %{targets}))'
    yield ')'
    yield '(rule'
    yield '  (targets JsOfOCairo.ml)'
    yield '  (deps (:interm JsOfOCairo.intermediate.ml))'
    yield '  (action (with-stdout-to %{targets} (run %{bin:sed} "s|# \\\\([0-9]\\\\+\\\\) \\"|# \\\\1 \\"src/|" %{interm})))'
    yield ')'
    yield ''
    yield '; Programs that link to js_of_ocaml cannot run as native or bytecode,'
    yield '; because js_of_ocaml provides C stubs of pure JavaScript functions that raise exceptions when called,'
    yield '; and one of them is called during initialization of js_of_ocaml.'
    yield '; We want CairoMock to be usable in native/bytecode applications.'
    yield '; So we use (wrapped false) to distribute the CairoMock module as a top-level by-product of JsOfOCairo.'
    yield '; We take care manually to not pollute the global namespace, as described in'
    yield '; https://dune.readthedocs.io/en/latest/dune-files.html#library'
    yield '(library'
    yield '  (name JsOfOCairo)'
    yield '  (public_name JsOfOCairo)'
    bisect_ppx = " bisect_ppx" if flavor == "coverage" else ""
    yield '  (preprocess (pps js_of_ocaml-ppx{}))'.format(bisect_ppx)
    yield '  (libraries js_of_ocaml)'
    yield '  (wrapped false)'
    yield ')'

for line in gen(sys.argv[1]):
    print(line)
