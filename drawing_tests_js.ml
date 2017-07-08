(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module Tests = DrawingTests.Make(JsOfOCairo)

let drawing_tests =
  Tests.tests
  |> Li.map ~f:(fun {Tests.name; width; height; draw} ->
    object%js (_)
      val name = Js.string name
      val width = width
      val height = height
      method draw context = draw (JsOfOCairo.create context)
    end
  )
  |> Li.to_array

let () = Js.export "drawing_tests" drawing_tests
