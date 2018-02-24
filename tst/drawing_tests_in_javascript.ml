(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open StdLabels

let () = Tests.MockTests.run ()

(* @todo Run DrawingTests on CairoMock *)

let drawing_tests =
  let module Tests = Tests.DrawingTests.Make(JsOfOCairo) in
  Tests.tests
  |> List.map ~f:(fun {Tests.name; width; height; draw; known_failure} ->
    object%js (_)
      val name = Js.string name
      val width = width
      val height = height
      method draw canvas = draw (JsOfOCairo.create canvas)
      val known_failure_ = Js.bool known_failure
    end
  )
  |> Array.of_list

let () = Js.export "drawing_tests" drawing_tests
