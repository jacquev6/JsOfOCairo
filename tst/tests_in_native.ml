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
  "Drawing tests on Cairo" >:: (
    let module T = Tests.Drawing.Make(Cairo) in
    T.tests
    |> Li.map ~f:(fun {T.name; width; height; draw} ->
      name >: (lazy (
        let img = Cairo.Image.create Cairo.Image.ARGB32 ~width ~height in
        let ctx = Cairo.create img in
        draw ctx;
        Cairo.PNG.write img (Frmt.apply "Tests/Drawing/Cairo/%s.png" name);
      ))
    )
  );
]

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv test)
