(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = sig
  #include "S.ml"
end

include S

val create: Dom_html.canvasElement Js.t -> context
