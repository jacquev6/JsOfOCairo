(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

module T = Tests.Make(struct
  let title = "Tests in native"

  module C = Cairo

  module N = struct
    let name = "Cairo"

    let degraded = false

    let create () =
      let img = Cairo.Image.create Cairo.Image.ARGB32 ~width:10 ~height:10 in
      Cairo.create img
  end

  module DrawingTest(T: sig
    type t = {name: string; width: int; height: int; draw: C.context -> unit}
  end) = struct
    let run {T.name; width; height; draw} =
      let img = Cairo.Image.create Cairo.Image.ARGB32 ~width ~height in
      let ctx = Cairo.create img in
      draw ctx;
      Cairo.PNG.write img (Frmt.apply "Tests/Drawing/Cairo/%s.png" name);
  end
end)

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv T.test)
