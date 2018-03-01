(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = sig
  #include "JsOfOCairo.signatures.ml"
end

include S

val create: Dom_html.canvasElement Js.t -> context
