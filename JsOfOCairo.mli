(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = JsOfOCairo_Context.S

include JsOfOCairo_Context.S

val create: Dom_html.canvasRenderingContext2D Js.t -> context
