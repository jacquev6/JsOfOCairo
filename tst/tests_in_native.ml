(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

let test = "Tests in native" >:: [
  Tests.Unit.test;
  (
    let module T = Tests.Universal.Make(Cairo)(struct
      let name = "Cairo"

      let degraded = false

      let create () =
        let img = Cairo.Image.create Cairo.Image.ARGB32 ~width:10 ~height:10 in
        Cairo.create img
    end) in
    T.test
  );
  (
    let module T = Tests.Universal.Make(CairoMock)(struct
      let name = "CairoMock"

      let degraded = false

      let create = CairoMock.create
    end) in
    T.test
  );
]

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv test)
