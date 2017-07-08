(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module M = struct
    type t = {xx: float; xy: float; yx: float; yy: float; dx: float; dy: float}

    let one = {xx=1.; xy=0.; yx=0.; yy=1.; dx=0.; dy=0.}

    let apply_dist {xx; xy; yx; yy; _} ~x ~y =
        let x = xx *. x +. xy *. y
        and y = yx *. x +. yy *. y
        in (x, y)

    let rev_apply_dist {xx; xy; yx; yy; _} ~x ~y =
        let d = xx *. yy -. xy *. yx in
        let xx = yy /. d
        and xy = -. xy /. d
        and yx = -. yx /. d
        and yy = xx /. d in
        apply_dist {xx; xy; yx; yy; dx=0.; dy=0.} ~x ~y

    let apply_point ({dx; dy; _} as m) ~x ~y =
        let (x, y) = apply_dist m ~x ~y in
        (x +. dx, y +. dy)

    let rev_apply_point ({dx; dy; _} as m) ~x ~y =
        let (x, y) = (x -. dx, y -. dy) in
        rev_apply_dist m ~x ~y

    let compose ({xx; xy; yx; yy; dx; dy} as m) {xx=xx'; xy=xy'; yx=yx'; yy=yy'; dx=dx'; dy=dy'} =
        let (dx', dy') = apply_dist m ~x:dx' ~y:dy' in
        let xx = xx *. xx' +. xy *. yx'
        and xy = xx *. xy' +. xy *. yy'
        and yx = yx *. xx' +. yy *. yx'
        and yy = yx *. xy' +. yy *. yy'
        and dx = dx +. dx'
        and dy = dy +. dy'
        in {xx; xy; yx; yy; dx; dy}
end

type context = {
  ctx: Dom_html.canvasRenderingContext2D Js.t;
  mutable start_point: float * float;
  mutable current_point: float * float;
  mutable transformation: M.t;
}

let set_line_width context width =
  context.ctx##.lineWidth := width

let get_line_width context =
  context.ctx##.lineWidth

let create ctx =
  let context = {
    ctx;
    start_point = (0., 0.);
    current_point = (0., 0.);
    transformation = M.one;
  } in
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

let device_to_user context ~x ~y =
  M.rev_apply_point context.transformation ~x ~y

let device_to_user_distance context ~x ~y =
  M.rev_apply_dist context.transformation ~x ~y

let user_to_device context ~x ~y =
  M.apply_point context.transformation ~x ~y

let user_to_device_distance context ~x ~y =
  M.apply_dist context.transformation ~x ~y

let transform_state context m =
    context.transformation <- M.compose context.transformation m;
    let (x, y) = context.current_point in
    context.current_point <- M.rev_apply_point m ~x ~y;
    let (x, y) = context.start_point in
    context.start_point <- M.rev_apply_point m ~x ~y

let scale context ~x ~y =
    context.ctx##scale x y;
    transform_state context {M.one with M.xx=x; yy=y}

let translate context ~x ~y =
    context.ctx##translate x y;
    transform_state context {M.one with M.dx=x; dy=y}

let rotate context ~angle =
    context.ctx##rotate angle;
    transform_state context {
        M.one with
        M.xx = Math.cos angle;
        xy = -. Math.sin angle;
        yx = Math.sin angle;
        yy = Math.cos angle;
    }

let identity_matrix context =
    let (x, y) = context.current_point in
    context.current_point <- M.apply_point context.transformation ~x ~y;
    let (x, y) = context.start_point in
    context.start_point <- M.apply_point context.transformation ~x ~y;
    context.transformation <- M.one;
    context.ctx##setTransform 1. 0. 0. 1. 0. 0.
