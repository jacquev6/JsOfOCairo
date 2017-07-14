(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module Tests = DrawingTests.Make(JsOfOCairo)

let drawing_tests =
  Tests.tests
  |> Li.map ~f:(fun {Tests.name; width; height; draw; known_failure} ->
    object%js (_)
      val name = Js.string name
      val width = width
      val height = height
      method draw canvas = draw (JsOfOCairo.create canvas)
      val known_failure_ = Js.bool known_failure
    end
  )
  |> Li.to_array

let () = Js.export "drawing_tests" drawing_tests
