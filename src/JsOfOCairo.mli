(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = CairoMock.S

include S

val create: Js_of_ocaml.Dom_html.canvasElement Js_of_ocaml.Js.t -> context
