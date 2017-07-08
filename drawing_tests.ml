(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module Tests = DrawingTests.Make(Cairo)

let () = Li.iter Tests.tests ~f:(fun {Tests.name; width; height; draw} ->
    let img = Cairo.Image.create Cairo.Image.ARGB32 ~width ~height in
    let ctx = Cairo.create img in
    draw ctx;
    Cairo.PNG.write img (OCamlStandard.Printf.sprintf "drawing_tests/%s.png" name);
)
