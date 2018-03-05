(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = sig
  #include "CairoMock.signatures.ml"
end

include S

val create: unit -> context

val calls: context -> string list

module Backend: module type of Backend
