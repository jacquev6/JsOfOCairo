(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = module type of JsOfOCairo_S

include S

val create: Dom_html.canvasElement Js.t -> context
