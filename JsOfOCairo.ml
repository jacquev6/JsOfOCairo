(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

type context = {
    ctx: Dom_html.canvasRenderingContext2D Js.t;
}

let set_line_width {ctx} width =
    ctx##.lineWidth := width

let get_line_width {ctx} =
    ctx##.lineWidth

let create ctx =
    let context = {ctx} in
    set_line_width context 2.0;
    context

let move_to {ctx} ~x ~y =
  ctx##moveTo x y

let line_to {ctx} ~x ~y =
  ctx##lineTo x y

let stroke {ctx} =
  ctx##stroke;
  ctx##beginPath
