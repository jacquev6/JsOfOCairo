(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open StdLabels

let () = Tests.MockTests.run ()

let drawing_tests =
  let module JsOfOCairoTests = Tests.DrawingTests.Make(JsOfOCairo) in
  let module CairoMockTests = Tests.DrawingTests.Make(CairoMock) in
  List.map2 JsOfOCairoTests.tests CairoMockTests.tests ~f:(fun {JsOfOCairoTests.name; width; height; draw; known_failure} {CairoMockTests.draw=draw_mock; _} ->
    let script =
      let ctx = CairoMock.create () in
      draw_mock ctx;
      CairoMock.calls ctx
    in
    object%js (_)
      val name = Js.string name
      val width = width
      val height = height
      method draw canvas = draw (JsOfOCairo.create canvas)
      val script = script |> List.map ~f:Js.string |> Array.of_list |> Js.array
      val known_failure_ = Js.bool known_failure
    end
  )
  |> Array.of_list
  |> Js.array

let () = Js.export "drawing_tests" drawing_tests
