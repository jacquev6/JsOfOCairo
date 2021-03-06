(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

module Drawings = Drawings.Make(JsOfOCairo)

let () = Js_of_ocaml.Js.export "draw" (fun canvas ->
  Drawings.draw (JsOfOCairo.create canvas)
)
