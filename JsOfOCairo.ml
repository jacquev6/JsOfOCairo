(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module type S = module type of JsOfOCairo_S

(* http://www.w3schools.com/tags/ref_canvas.asp *)

type matrix = {
  mutable xx: float;
  mutable yx: float;
  mutable xy: float;
  mutable yy: float;
  mutable x0: float;
  mutable y0: float;
}

module Matrix = struct
  type t = matrix


  let transform_distance' {xx; xy; yx; yy; _} (x, y) =
    let x = xx *. x +. xy *. y
    and y = yx *. x +. yy *. y
    in (x, y)

  let transform_point' ({x0; y0; _} as m) (x, y) =
    let (x, y) = transform_distance' m (x, y) in
    (x +. x0, y +. y0)

  let rev_transform_distance' {xx; xy; yx; yy; _} (x, y) =
    let d = xx *. yy -. xy *. yx in
    let xx = yy /. d
    and xy = -. xy /. d
    and yx = -. yx /. d
    and yy = xx /. d in
    transform_distance' {xx; xy; yx; yy; x0=0.; y0=0.} (x, y)

  let rev_transform_point' ({x0; y0; _} as m) (x, y) =
    let (x, y) = (x -. x0, y -. y0) in
    rev_transform_distance' m (x, y)


  let init_identity () =
    {xx=1.; xy=0.; yx=0.; yy=1.; x0=0.; y0=0.}

  let init_translate ~x ~y =
    {xx=1.; xy=0.; yx=0.; yy=1.; x0=x; y0=y}

  let init_scale ~x ~y =
    {xx=x; xy=0.; yx=0.; yy=y; x0=0.; y0=0.}

  let init_rotate ~angle =
    {
      xx = Math.cos angle;
      xy = -. Math.sin angle;
      yx = Math.sin angle;
      yy = Math.cos angle;
      x0 = 0.;
      y0 = 0.;
    }

  let transform_distance m ~dx ~dy =
    transform_distance' m (dx, dy)

  let transform_point m ~x ~y =
    transform_point' m (x, y)

  let multiply ({xx; xy; yx; yy; x0; y0} as m) {xx=xx'; xy=xy'; yx=yx'; yy=yy'; x0=x0'; y0=y0'} =
    let (x0', y0') = transform_distance' m (x0', y0') in
    let xx = xx *. xx' +. xy *. yx'
    and xy = xx *. xy' +. xy *. yy'
    and yx = yx *. xx' +. yy *. yx'
    and yy = yx *. xy' +. yy *. yy'
    and x0 = x0 +. x0'
    and y0 = y0 +. y0'
    in {xx; xy; yx; yy; x0; y0}
end

type context = {
  ctx: Dom_html.canvasRenderingContext2D Js.t;
  mutable start_point: (float * float) option;
  mutable current_point: float * float;
  mutable transformation: Matrix.t;
  mutable saved_transformations: Matrix.t list;
}

let set_line_width context width =
  context.ctx##.lineWidth := width

let get_line_width context =
  context.ctx##.lineWidth

let create canvas =
  let ctx = canvas##getContext Dom_html._2d_ in
  let context = {
    ctx;
    start_point = None;
    current_point = (0., 0.);
    transformation = Matrix.init_identity ();
    saved_transformations = [];
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
    context.current_point <- Opt.value context.start_point
end

let move_to context ~x ~y =
  context.ctx##moveTo x y;
  context.current_point <- (x, y);
  context.start_point <- Some (x, y)

let line_to context ~x ~y =
  context.ctx##lineTo x y;
  context.current_point <- (x, y)

let arc context ~x ~y ~r ~a1 ~a2 =
  context.ctx##arc x y r a1 a2 Js._false;
  if Opt.is_none context.start_point then
  context.start_point <- Some (x +. r *. (Math.cos a1), y +. r *. (Math.sin a1));
  context.current_point <- (x +. r *. (Math.cos a2), y +. r *. (Math.sin a2))

let arc_negative context ~x ~y ~r ~a1 ~a2 =
  context.ctx##arc x y r a1 a2 Js._true;
  if Opt.is_none context.start_point then
  context.start_point <- Some (x +. r *. (Math.cos a1), y +. r *. (Math.sin a1));
  context.current_point <- (x +. r *. (Math.cos a2), y +. r *. (Math.sin a2))

let stroke_preserve context =
  context.ctx##stroke

let stroke context =
  stroke_preserve context;
  Path.clear context

let fill_preserve context =
  context.ctx##fill

let fill context =
  fill_preserve context;
  Path.clear context

let clip_preserve context =
  context.ctx##clip

let clip context =
  clip_preserve context;
  Path.clear context

let set_source_rgb context ~r ~g ~b =
  let convert x = OCamlStandard.Printf.sprintf "%02x" (Int.of_float (255.0 *. x)) in
  let color = Js.string (OCamlStandard.Printf.sprintf "#%s%s%s" (convert r) (convert g) (convert b)) in
  context.ctx##.fillStyle := color;
  context.ctx##.strokeStyle := color

let device_to_user context ~x ~y =
  Matrix.rev_transform_point' context.transformation (x, y)

let device_to_user_distance context ~x ~y =
  Matrix.rev_transform_distance' context.transformation (x, y)

let user_to_device context ~x ~y =
  Matrix.transform_point' context.transformation (x, y)

let user_to_device_distance context ~x ~y =
  Matrix.transform_distance' context.transformation (x, y)

let set_matrix context ({xx; xy; yx; yy; x0; y0} as m) =
  context.ctx##setTransform xx yx xy yy x0 y0;
  context.current_point <-
    context.current_point
    |> Matrix.transform_point' context.transformation
    |> Matrix.rev_transform_point' m;
  context.start_point <-
    context.start_point
    |> Opt.map ~f:(Matrix.transform_point' context.transformation)
    |> Opt.map ~f:(Matrix.rev_transform_point' m);
  context.transformation <- m

let get_matrix context =
  context.transformation

let transform context m =
  set_matrix context (Matrix.multiply context.transformation m)

let scale context ~x ~y =
  transform context (Matrix.init_scale ~x ~y)

let translate context ~x ~y =
  transform context (Matrix.init_translate ~x ~y)

let rotate context ~angle =
  transform context (Matrix.init_rotate ~angle)

let identity_matrix context =
  set_matrix context (Matrix.init_identity ())

let save context =
  context.ctx##save;
  context.saved_transformations <- context.transformation::context.saved_transformations

let restore context =
  context.ctx##restore;
  set_matrix context (Li.head context.saved_transformations);
  context.saved_transformations <- Li.tail context.saved_transformations;

type line_cap = BUTT | ROUND | SQUARE

let set_line_cap context cap =
  let cap = match cap with
    | BUTT -> "butt"
    | ROUND -> "round"
    | SQUARE -> "square"
  in
  context.ctx##.lineCap := Js.string cap

let get_line_cap context =
  match Js.to_string context.ctx##.lineCap with
    | "round" -> ROUND
    | "square" -> SQUARE
    | _ -> BUTT

type line_join = JOIN_MITER | JOIN_ROUND | JOIN_BEVEL

let set_line_join context join =
  let join = match join with
    | JOIN_MITER ->  "miter"
    | JOIN_ROUND -> "round"
    | JOIN_BEVEL -> "bevel"
  in
  context.ctx##.lineJoin := Js.string join

let get_line_join context =
  match Js.to_string context.ctx##.lineJoin with
    | "round" -> JOIN_ROUND
    | "bevel" -> JOIN_BEVEL
    | _ -> JOIN_MITER

let set_miter_limit context l =
  context.ctx##.miterLimit := l

let get_miter_limit context =
  context.ctx##.miterLimit

let make_rel context ~x:dx ~y:dy =
  let (x, y) = context.current_point in
  (x +. dx, y +. dy)

let rel_move_to context ~x ~y =
  let (x, y) = make_rel context ~x ~y in
  move_to context ~x ~y

let rel_line_to context ~x ~y =
  let (x, y) = make_rel context ~x ~y in
  line_to context ~x ~y

let curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  context.current_point <- (x3, y3);
  context.ctx##bezierCurveTo x1 y1 x2 y2 x3 y3

let rel_curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  let (x1, y1) = make_rel context ~x:x1 ~y:y1
  and (x2, y2) = make_rel context ~x:x2 ~y:y2
  and (x3, y3) = make_rel context ~x:x3 ~y:y3 in
  curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

let rectangle context ~x ~y ~w ~h =
  context.current_point <- (x, y);
  context.ctx##rect x y w h

type font_extents = {
  ascent: float;
  descent: float;
  baseline: float;
  max_x_advance: float;
  max_y_advance: float;
}

type text_extents = {
  x_bearing: float;
  y_bearing: float;
  width: float;
  height: float;
  x_advance: float;
  y_advance: float;
}

type slant = Upright | Italic | Oblique

type weight = Normal | Bold

type _font = {
  slant: slant;
  weight: weight;
  size: float;
  family: string;
}

let _set_font context {slant; weight; size; family} =
  let font_style = match slant with
    | Upright -> "normal"
    | Italic -> "italic"
    | Oblique -> "oblique"
  and font_weight = match weight with
    | Normal -> "normal"
    | Bold -> "bold"
  in
  let font = OCamlStandard.Printf.sprintf "%s %s %npx %s" font_style font_weight (Int.of_float size) family in
  context.ctx##.font := Js.string font

let _get_font context =
  (* @todo Test performance. This looks costly in DrawGrammar: we change fonts a lot, for each Token, Terminal, and NonTerminal.
  We could cache this data in the context, and invalidate the cache on C.restore *)
  context.ctx##.font
  |> Js.to_string
  |> Str.split ~sep:" "
  |> Li.fold ~init:{slant=Upright; weight=Normal; size=10.; family="sans-serif"} ~f:(fun font -> function
    | "normal" -> font
    | "italic" -> {font with slant=Italic}
    | "oblique" -> {font with slant=Oblique}
    | "bold" -> {font with weight=Bold}
    | part -> begin
      match Str.try_drop_suffix part ~suf:"px" with
        | None -> {font with family=part}
        | Some size -> {font with size=Fl.of_string size}
    end
  )

let select_font_face context ?(slant=Upright) ?(weight=Normal) family =
  _set_font context {(_get_font context) with slant; weight; family}

let set_font_size context size =
  _set_font context {(_get_font context) with size}

let show_text context s =
  let (x, y) = context.current_point in
  context.ctx##fillText (Js.string s) x y

let font_extents context =
  let {size; _} = _get_font context in
  {
    ascent = size;
    descent = size /. 4.;
    baseline = 0.;
    max_x_advance = 2. *. size;
    max_y_advance = 0.;
  }

let text_extents context s =
  let {size; _} = _get_font context
  and w = (context.ctx##measureText (Js.string s))##.width in
  {
    x_bearing = 0.;
    y_bearing = 0.;
    width = w;
    height = size;
    x_advance = w;
    y_advance = 0.;
  }

let paint ?alpha:_ context =
  save context;
  identity_matrix context;
  let width = (float_of_int context.ctx##.canvas##.width)
  and height = (float_of_int context.ctx##.canvas##.height) in
  (* @todo Implement alpha with something like:
  context.ctx##.fillStyle := Js.string "rgba(0, 255, 255, 0.5)";
  But this needs to be more general: we should handle all kinds of sources. *)
  context.ctx##fillRect 0. 0. width height;
  restore context
