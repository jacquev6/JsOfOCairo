(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

(* @todo Could we do "Canvas = require('canvas')" in this .ml file instead of relying on the test_in_node.js stub?
I tried let (canvas: ...) = Js.Unsafe.global##require (Js.string "canvas") but I got an error saying that ".require" is not a function. *)

let (canvas: (int -> int -> Dom_html.canvasElement Js.t) Js.constr) =
  Js.Unsafe.global##.Canvas

let test = "Tests in node.js" >:: [
  Tests.Unit.test;
  (
    let module T = Tests.Universal.Make(JsOfOCairo)(struct
      let name = "JsOfOCairo"

      let degraded = true

      let create () =
        JsOfOCairo.create (new%js canvas 10 10)
    end) in
    T.test
  );
]

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv test)
