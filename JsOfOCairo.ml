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

let arc {ctx} ~x ~y ~r ~a1 ~a2 =
  ctx##arc x y r a1 a2 Js._false

let arc_negative {ctx} ~x ~y ~r ~a1 ~a2 =
  ctx##arc x y r a1 a2 Js._true

let stroke {ctx} =
  ctx##stroke;
  ctx##beginPath

let stroke_preserve {ctx} =
  ctx##stroke

let fill {ctx} =
  ctx##fill;
  ctx##beginPath

let fill_preserve {ctx} =
  ctx##fill
