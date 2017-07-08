(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

type context = {
  ctx: Dom_html.canvasRenderingContext2D Js.t;
  mutable start_point: float * float;
  mutable current_point: float * float;
}

let set_line_width context width =
  context.ctx##.lineWidth := width

let get_line_width context =
  context.ctx##.lineWidth

let create ctx =
  let context = {ctx; start_point=(0., 0.); current_point=(0., 0.)} in
  set_line_width context 2.0;
  context

module Path = struct
  let get_current_point {current_point; _} =
    current_point

  let clear context =
    context.ctx##beginPath;
    context.current_point <- (0., 0.)

  let close context =
    context.ctx##closePath;
    context.current_point <- context.start_point
end

let move_to context ~x ~y =
  context.ctx##moveTo x y;
  context.current_point <- (x, y);
  context.start_point <- (x, y)

let line_to context ~x ~y =
  context.ctx##lineTo x y;
  context.current_point <- (x, y)

let arc context ~x ~y ~r ~a1 ~a2 =
  context.ctx##arc x y r a1 a2 Js._false;
  context.start_point <- (x +. r *. (Math.cos a1), y +. r *. (Math.sin a1));
  context.current_point <- (x +. r *. (Math.cos a2), y +. r *. (Math.sin a2))

let arc_negative context ~x ~y ~r ~a1 ~a2 =
  context.ctx##arc x y r a1 a2 Js._true;
  context.start_point <- (x +. r *. (Math.cos a1), y +. r *. (Math.sin a1));
  context.current_point <- (x +. r *. (Math.cos a2), y +. r *. (Math.sin a2))

let stroke context =
  context.ctx##stroke;
  Path.clear context

let stroke_preserve context =
  context.ctx##stroke

let fill context =
  context.ctx##fill;
  Path.clear context

let fill_preserve context =
  context.ctx##fill

let set_source_rgb context ~r ~g ~b =
  let convert x = OCamlStandard.Printf.sprintf "%02x" (Int.of_float (255.0 *. x)) in
  let color = Js.string (OCamlStandard.Printf.sprintf "#%s%s%s" (convert r) (convert g) (convert b)) in
  context.ctx##.fillStyle := color;
  context.ctx##.strokeStyle := color
