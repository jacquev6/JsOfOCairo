(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open StdLabels

let () = Tests.MockTests.run ()

let () =
  let module Tests = Tests.DrawingTests.Make(CairoMock) in
  Tests.tests
  |> List.iter ~f:(fun {Tests.name; draw; _} ->
    try (* @todo Remove when CairoMock is fully implemented *)
      draw (CairoMock.create ())
    with
      Failure s ->
        Printf.printf "%s: %s\n" name s
  )

let () =
  let module Tests = Tests.DrawingTests.Make(Cairo) in
  Tests.tests
  |> List.iter ~f:(fun {Tests.name; width; height; draw; known_failure=_} ->
    let img = Cairo.Image.create Cairo.Image.ARGB32 ~width ~height in
    let ctx = Cairo.create img in
    draw ctx;
    Cairo.PNG.write img (Printf.sprintf "drawing_tests/in_command_line/%s.png" name);
  )
