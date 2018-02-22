(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open StdLabels

module type S = sig
  #include "JsOfOCairo.signatures.ml"
end

(*
Canvas:
- http://www.w3schools.com/tags/ref_canvas.asp
- https://ocsigen.org/js_of_ocaml/2.8.4/api/Dom_html.canvasRenderingContext2D-c

Cairo:
- http://cairo.forge.ocamlcore.org/tutorial/Cairo.html
- https://github.com/Chris00/ocaml-cairo
- utop -require cairo2
*)

type status = INVALID_RESTORE | INVALID_POP_GROUP | NO_CURRENT_POINT | INVALID_MATRIX | INVALID_STATUS | NULL_POINTER | INVALID_STRING | INVALID_PATH_DATA | READ_ERROR | WRITE_ERROR | SURFACE_FINISHED | SURFACE_TYPE_MISMATCH | PATTERN_TYPE_MISMATCH | INVALID_CONTENT | INVALID_FORMAT | INVALID_VISUAL | FILE_NOT_FOUND | INVALID_DASH | INVALID_DSC_COMMENT | INVALID_INDEX | CLIP_NOT_REPRESENTABLE | TEMP_FILE_ERROR | INVALID_STRIDE | FONT_TYPE_MISMATCH | USER_FONT_IMMUTABLE | USER_FONT_ERROR | NEGATIVE_COUNT | INVALID_CLUSTERS | INVALID_SLANT | INVALID_WEIGHT | INVALID_SIZE | USER_FONT_NOT_IMPLEMENTED | DEVICE_TYPE_MISMATCH | DEVICE_ERROR | INVALID_MESH_CONSTRUCTION | DEVICE_FINISHED | JBIG2_GLOBAL_MISSING

exception Error of status

let status_to_string = function
  | INVALID_RESTORE -> "cairo_restore() without matching cairo_save()"
  | INVALID_POP_GROUP -> "no saved group to pop, i.e. cairo_pop_group() without matching cairo_push_group()"
  | NO_CURRENT_POINT -> "no current point defined"
  | INVALID_MATRIX -> "invalid matrix (not invertible)"
  | INVALID_STATUS -> "invalid value for an input cairo_status_t"
  | NULL_POINTER -> "NULL pointer"
  | INVALID_STRING -> "input string not valid UTF-8"
  | INVALID_PATH_DATA -> "input path data not valid"
  | READ_ERROR -> "error while reading from input stream"
  | WRITE_ERROR -> "error while writing to output stream"
  | SURFACE_FINISHED -> "the target surface has been finished"
  | SURFACE_TYPE_MISMATCH -> "the surface type is not appropriate for the operation"
  | PATTERN_TYPE_MISMATCH -> "the pattern type is not appropriate for the operation"
  | INVALID_CONTENT -> "invalid value for an input cairo_content_t"
  | INVALID_FORMAT -> "invalid value for an input cairo_format_t"
  | INVALID_VISUAL -> "invalid value for an input Visual*"
  | FILE_NOT_FOUND -> "file not found"
  | INVALID_DASH -> "invalid value for a dash setting"
  | INVALID_DSC_COMMENT -> "invalid value for a DSC comment"
  | INVALID_INDEX -> "invalid index passed to getter"
  | CLIP_NOT_REPRESENTABLE -> "clip region not representable in desired format"
  | TEMP_FILE_ERROR -> "error creating or writing to a temporary file"
  | INVALID_STRIDE -> "invalid value for stride"
  | FONT_TYPE_MISMATCH -> "the font type is not appropriate for the operation"
  | USER_FONT_IMMUTABLE -> "the user-font is immutable"
  | USER_FONT_ERROR -> "error occurred in a user-font callback function"
  | NEGATIVE_COUNT -> "negative number used where it is not allowed"
  | INVALID_CLUSTERS -> "input clusters do not represent the accompanying text and glyph arrays"
  | INVALID_SLANT -> "invalid value for an input cairo_font_slant_t"
  | INVALID_WEIGHT -> "invalid value for an input cairo_font_weight_t"
  | INVALID_SIZE -> "invalid value (typically too big) for the size of the input (surface, pattern, etc.)"
  | USER_FONT_NOT_IMPLEMENTED -> "user-font method not implemented"
  | DEVICE_TYPE_MISMATCH -> "the device type is not appropriate for the operation"
  | DEVICE_ERROR -> "an operation to the device caused an unspecified error"
  | INVALID_MESH_CONSTRUCTION -> "invalid operation during mesh pattern construction"
  | DEVICE_FINISHED -> "the target device has been finished"
  | JBIG2_GLOBAL_MISSING -> "CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID used but no CAIRO_MIME_TYPE_JBIG2_GLOBAL data provided"

exception Unavailable

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

  (*
    2 by 2 matrix with (x0, y0) offset is equivalent to the following 3 by 3 matrix:
    / xx xy x0 \   / x \   / xx*x + xy*y + x0 \
    | yx yy y0 | * | y | = | yx*x + yy*y + y0 |
    \  0  0  1 /   \ 1 /   \                1 /
  *)

  let init_identity () =
    {xx=1.; xy=0.; yx=0.; yy=1.; x0=0.; y0=0.}

  let init_translate ~x ~y =
    {xx=1.; xy=0.; yx=0.; yy=1.; x0=x; y0=y}

  let init_scale ~x ~y =
    {xx=x; xy=0.; yx=0.; yy=y; x0=0.; y0=0.}

  let init_rotate ~angle =
    {
      xx = cos angle;
      xy = -.sin angle;
      yx = sin angle;
      yy = cos angle;
      x0 = 0.;
      y0 = 0.;
    }

  let init_inverse {xx; xy; yx; yy; x0; y0} =
    let d = xx *. yy -. xy *. yx in
    let xx = yy /. d
    and xy = -. xy /. d
    and yx = -. yx /. d
    and yy = xx /. d
    and x0 = (xy *. y0 -. yy *. x0) /. d
    and y0 = (yx *. x0 -. xx *. y0) /. d in
    {xx; xy; yx; yy; x0; y0}

  let multiply {xx; xy; yx; yy; x0; y0} {xx=xx'; xy=xy'; yx=yx'; yy=yy'; x0=x0'; y0=y0'} =
    let xx = xx *. xx' +. xy *. yx'
    and xy = xx *. xy' +. xy *. yy'
    and yx = yx *. xx' +. yy *. yx'
    and yy = yx *. xy' +. yy *. yy'
    and x0 = xx *. x0' +. xy *. y0' +. x0
    and y0 = yx *. x0' +. yy *. y0' +. y0 in
    {xx; xy; yx; yy; x0; y0}

  let apply {xx; xy; yx; yy; x0; y0} (x, y) =
    (xx *. x +. xy *. y +. x0, yx *. x +. yy *. y +. y0)

  let transform_point m ~x ~y =
    apply m (x, y)

  let transform_distance {xx; xy; yx; yy; x0=_; y0=_} ~dx ~dy =
    (xx *. dx +. xy *. dy, yx *. dx +. yy *. dy)

  let set m {xx; xy; yx; yy; x0; y0} =
    m.xx <- xx;
    m.xy <- xy;
    m.yx <- yx;
    m.yy <- yy;
    m.x0 <- x0;
    m.y0 <- y0

  let scale m ~x ~y =
    set m (multiply m (init_scale ~x ~y))

  let translate m ~x ~y =
    set m (multiply m (init_translate ~x ~y))

  let rotate m ~angle =
    set m (multiply m (init_rotate ~angle))

  let invert m =
    set m (init_inverse m)
end

type slant = Upright | Italic | Oblique

type weight = Normal | Bold

type font = {
  slant: slant;
  weight: weight;
  size: float;
  family: string;
}

module Pattern = struct
  module StopPointList: sig
    type t
    val empty: t
    val size: t -> int
    val add: t -> float * float * float * float * float -> t
    val iter: t -> f:(float * float * float * float * float -> unit) -> unit
    val get: t -> i:int -> float * float * float * float * float
  end = struct
    module Element = struct
      type t =
        float (* position *)
        * int (* index added (used to sort stop points added at the same position) *)
        * float * float * float * float (* r, g, b, a *)

      let compare (a_pos, a_idx, _, _, _, _) (b_pos, b_idx, _, _, _, _) =
        match compare a_pos b_pos with
          | 0 -> compare a_idx b_idx
          | n -> n
    end

    type t = Element.t list

    let empty = []

    let size = List.length

    let add xs (position, r, g, b, a) =
      let element = (position, List.length xs, r, g, b, a) in
      let rec aux = function
        | [] -> [element]
        | x::xs as xxs -> match Element.compare element x with
          | -1 | 0 -> element::xxs
          | _ -> x::(aux xs)
      in aux xs

    let iter xs ~f =
      List.iter xs ~f:(fun (position, _, r, g, b, a) -> f (position, r, g, b, a))

    let get xs ~i =
      let (position, _, r, g, b, a) = List.nth xs i in
      (position, r, g, b, a)
  end

  type source =
    | Rgba of float * float * float * float
    | LinearGradient of (float * float * float * float) * StopPointList.t
    | RadialGradient of (float * float * float * float * float * float) * StopPointList.t

  type 'a t = source ref constraint 'a = [<`Solid | `Surface | `Gradient | `Linear | `Radial]

  type any = [`Solid | `Surface | `Gradient | `Linear | `Radial] t

  let create_rgba ~r ~g ~b ~a =
    ref (Rgba (r, g, b, a))

  let create_rgb ~r ~g ~b =
    create_rgba ~r ~g ~b ~a:1.

  let get_rgba pattern =
    match !pattern with
      | Rgba (r, g, b, a) -> (r, g, b, a)
      | LinearGradient _ | RadialGradient _ -> raise (Error PATTERN_TYPE_MISMATCH)

  let create_linear ~x0 ~y0 ~x1 ~y1 =
    ref (LinearGradient ((x0, y0, x1, y1), StopPointList.empty))

  let get_linear_points pattern =
    match !pattern with
      | LinearGradient (points, _) -> points
      | Rgba _ | RadialGradient _ -> raise (Error PATTERN_TYPE_MISMATCH)

  let create_radial ~x0 ~y0 ~r0 ~x1 ~y1 ~r1 =
    ref (RadialGradient ((x0, y0, r0, x1, y1, r1), StopPointList.empty))

  let get_radial_circles pattern =
    match !pattern with
      | RadialGradient (circles, _) -> circles
      | LinearGradient _ | Rgba _ -> raise (Error PATTERN_TYPE_MISMATCH)

  let add_color_stop_rgba pattern ?(ofs=0.) r g b a =
    match !pattern with
      | LinearGradient (points, stops) ->
        pattern := LinearGradient (points, StopPointList.add stops (ofs, r, g, b, a))
      | RadialGradient (circles, stops) ->
        pattern := RadialGradient (circles, StopPointList.add stops (ofs, r, g, b, a))
      | Rgba _ -> raise (Error PATTERN_TYPE_MISMATCH)

  let add_color_stop_rgb pattern ?ofs r g b =
    add_color_stop_rgba pattern ?ofs r g b 1.

  let get_color_stop_count pattern =
    match !pattern with
      | LinearGradient (_, stops) | RadialGradient (_, stops) -> StopPointList.size stops
      | Rgba _ -> raise (Error PATTERN_TYPE_MISMATCH)

  let get_color_stop_rgba pattern ~idx =
    match !pattern with
      | LinearGradient (_, stops) | RadialGradient (_, stops) -> StopPointList.get stops ~i:idx
      | Rgba _ -> raise (Error PATTERN_TYPE_MISMATCH)
end

type fill_rule = WINDING | EVEN_ODD

module Html = struct
  type t = Dom_html.canvasRenderingContext2D Js.t
end

module State = struct
  type t = {
    transformation: Matrix.t;
    font: font;
    source: Pattern.source;
    fill_rule: fill_rule;
  }
end

type context = {
  html: Html.t;
  mutable start_point: (float * float) option;
  mutable current_point: (float * float) option;
  mutable states: State.t list;
}

let get_state context =
  List.hd context.states

let set_state ?transformation ?font ?source ?fill_rule context =
  let state = get_state context in
  let state = match transformation with None -> state | Some transformation -> {state with transformation} in
  let state = match font with None -> state | Some font -> {state with font} in
  let state = match source with None -> state | Some source -> {state with source} in
  let state = match fill_rule with None -> state | Some fill_rule -> {state with fill_rule} in
  context.states <- state::(List.tl context.states)

let set_current_point context (x, y) =
  context.current_point <- Some (Matrix.transform_point (get_state context).transformation ~x ~y)

let set_start_point_as_current_point context =
  context.current_point <- context.start_point

let reset_current_point context =
  context.current_point <- None

let set_start_point context (x, y) =
  context.start_point <- Some (Matrix.transform_point (get_state context).transformation ~x ~y)

let set_start_point_if_none context p =
  match context.start_point with
    | None ->
      set_start_point context p
    | Some _ ->
      ()

let reset_start_point context =
  context.start_point <- None

let set_line_width context width =
  context.html##.lineWidth := width

let get_line_width context =
  context.html##.lineWidth

let set_dash context ?(ofs=0.) dashes =
  let html = Js.Unsafe.coerce context.html in
  html##.lineDashOffset := ofs;
  html##setLineDash (Js.array dashes)

let get_dash context =
  let html = Js.Unsafe.coerce context.html in
  (Js.to_array (html##getLineDash), html##.lineDashOffset)

let move_to context ~x ~y =
  context.html##moveTo x y;
  set_start_point context (x, y);
  set_start_point_as_current_point context

let line_to context ~x ~y =
  context.html##lineTo x y;
  set_start_point_if_none context (x, y);
  set_current_point context (x, y)

let arc context ~x ~y ~r ~a1 ~a2 =
  context.html##arc x y r a1 a2 Js._false;
  set_start_point_if_none context (x +. r *. (cos a1), y +. r *. (sin a1));
  set_current_point context (x +. r *. (cos a2), y +. r *. (sin a2))

let arc_negative context ~x ~y ~r ~a1 ~a2 =
  context.html##arc x y r a1 a2 Js._true;
  set_start_point_if_none context (x +. r *. (cos a1), y +. r *. (sin a1));
  set_current_point context (x +. r *. (cos a2), y +. r *. (sin a2))

let set_source context pattern =
  let convert x = string_of_int (int_of_float (255.0 *. x)) in
  let convert_rgba r g b a = Js.string (Printf.sprintf "rgba(%s, %s, %s, %f)" (convert r) (convert g) (convert b) a) in
  let source = !pattern in
  set_state context ~source;
  match source with
    | Pattern.Rgba (r, g, b, a) ->
      let color = convert_rgba r g b a in
      context.html##.fillStyle := color;
      context.html##.strokeStyle := color
    | Pattern.LinearGradient ((x0, y0, x1, y1), stops) ->
      let gradient = context.html##createLinearGradient x0 y0 x1 y1 in
      stops
      |> Pattern.StopPointList.iter ~f:(fun (ofs, r, g, b, a) ->
        gradient##addColorStop ofs (convert_rgba r g b a)
      );
      context.html##.fillStyle_gradient := gradient;
      context.html##.strokeStyle_gradient := gradient
    | Pattern.RadialGradient ((x0, y0, r0, x1, y1, r1), stops) ->
      let gradient = context.html##createRadialGradient x0 y0 r0 x1 y1 r1 in
      stops
      |> Pattern.StopPointList.iter ~f:(fun (ofs, r, g, b, a) ->
        gradient##addColorStop ofs (convert_rgba r g b a)
      );
      context.html##.fillStyle_gradient := gradient;
      context.html##.strokeStyle_gradient := gradient

let get_source context =
  ref (get_state context).source

let set_source_rgb context ~r ~g ~b =
  set_source context (Pattern.create_rgb ~r ~g ~b)

let set_source_rgba context ~r ~g ~b ~a =
  set_source context (Pattern.create_rgba ~r ~g ~b ~a)

let device_to_user context ~x ~y =
  Matrix.transform_point (Matrix.init_inverse (get_state context).transformation) ~x ~y

let device_to_user_distance context ~x ~y =
  Matrix.transform_distance (Matrix.init_inverse (get_state context).transformation) ~dx:x ~dy:y

let user_to_device context ~x ~y =
  Matrix.transform_point (get_state context).transformation ~x ~y

let user_to_device_distance context ~x ~y =
  Matrix.transform_distance (get_state context).transformation ~dx:x ~dy:y

module Path = struct
  let get_current_point ({current_point; _} as context) =
    match current_point with
      | None ->
        (0., 0.)
      | Some (x, y) ->
        device_to_user context ~x ~y

  let clear context =
    context.html##beginPath;
    reset_start_point context;
    reset_current_point context

  let close context =
    context.html##closePath;
    set_start_point_as_current_point context
end

let stroke_preserve context =
  context.html##stroke

let stroke context =
  stroke_preserve context;
  Path.clear context

let set_fill_rule context fill_rule =
  set_state context ~fill_rule

let get_fill_rule context =
  (get_state context).fill_rule

let fill_preserve context =
  match (get_state context).fill_rule with
    | WINDING -> context.html##fill
    | EVEN_ODD -> (Js.Unsafe.coerce context.html)##fill (Js.string "evenodd")

let fill context =
  fill_preserve context;
  Path.clear context

let clip_preserve context =
  context.html##clip

let clip context =
  clip_preserve context;
  Path.clear context

let set_matrix context ({xx; xy; yx; yy; x0; y0} as m) =
  context.html##setTransform xx yx xy yy x0 y0;
  set_state context ~transformation:m

let get_matrix context =
  (get_state context).transformation

let transform context m =
  set_matrix context (Matrix.multiply (get_state context).transformation m)

let scale context ~x ~y =
  transform context (Matrix.init_scale ~x ~y)

let translate context ~x ~y =
  transform context (Matrix.init_translate ~x ~y)

let rotate context ~angle =
  transform context (Matrix.init_rotate ~angle)

let identity_matrix context =
  set_matrix context (Matrix.init_identity ())

let save context =
  context.html##save;
  context.states <- (get_state context)::context.states

type line_cap = BUTT | ROUND | SQUARE

let set_line_cap context cap =
  let cap = match cap with
    | BUTT -> "butt"
    | ROUND -> "round"
    | SQUARE -> "square"
  in
  context.html##.lineCap := Js.string cap

let get_line_cap context =
  match Js.to_string context.html##.lineCap with
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
  context.html##.lineJoin := Js.string join

let get_line_join context =
  match Js.to_string context.html##.lineJoin with
    | "round" -> JOIN_ROUND
    | "bevel" -> JOIN_BEVEL
    | _ -> JOIN_MITER

let set_miter_limit context l =
  context.html##.miterLimit := l

let get_miter_limit context =
  context.html##.miterLimit

let make_rel context ~x:dx ~y:dy =
  match context.current_point with
    | None -> raise (Error NO_CURRENT_POINT)
    | Some (x, y) -> (x +. dx, y +. dy)

let rel_move_to context ~x ~y =
  let (x, y) = make_rel context ~x ~y in
  move_to context ~x ~y

let rel_line_to context ~x ~y =
  let (x, y) = make_rel context ~x ~y in
  line_to context ~x ~y

let curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  context.html##bezierCurveTo x1 y1 x2 y2 x3 y3;
  set_start_point_if_none context (x1, y1);
  set_current_point context (x3, y3)

let rel_curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  let (x1, y1) = make_rel context ~x:x1 ~y:y1
  and (x2, y2) = make_rel context ~x:x2 ~y:y2
  and (x3, y3) = make_rel context ~x:x3 ~y:y3 in
  curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

let rectangle context ~x ~y ~w ~h =
  set_current_point context (x, y);
  context.html##rect x y w h

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

let _set_font context ({slant; weight; size; family} as font) =
  set_state context ~font;
  let font_style = match slant with
    | Upright -> "normal"
    | Italic -> "italic"
    | Oblique -> "oblique"
  and font_weight = match weight with
    | Normal -> "normal"
    | Bold -> "bold"
  in
  let font = Printf.sprintf "%s %s %npx %s" font_style font_weight (int_of_float size) family in
  context.html##.font := Js.string font

let restore context =
  context.html##restore;
  let states =
    match context.states with
      | [] | [_] -> raise (Error INVALID_RESTORE)
      | _::states -> states
  in
  context.states <- states

let select_font_face context ?(slant=Upright) ?(weight=Normal) family =
  _set_font context {(get_state context).font with slant; weight; family}

let set_font_size context size =
  _set_font context {(get_state context).font with size}

let show_text context s =
  let (x, y) = Path.get_current_point context in
  context.html##fillText (Js.string s) x y

let font_extents context =
  let {size; _} = (get_state context).font in
  {
    ascent = size;
    descent = size /. 4.;
    baseline = 0.;
    max_x_advance = 2. *. size;
    max_y_advance = 0.;
  }

let text_extents context s =
  let {size; _} = (get_state context).font
  and w = (context.html##measureText (Js.string s))##.width in
  {
    x_bearing = 0.;
    y_bearing = 0.;
    width = w;
    height = size;
    x_advance = w;
    y_advance = 0.;
  }

let paint ?(alpha=1.) context =
  save context;
  context.html##.globalAlpha := alpha;
  identity_matrix context;
  let width = (float_of_int context.html##.canvas##.width)
  and height = (float_of_int context.html##.canvas##.height) in
  context.html##fillRect 0. 0. width height;
  restore context

let create canvas =
  let html = canvas##getContext Dom_html._2d_ in
  let context = {
    html;
    start_point = None;
    current_point = None;
    states = [
      {
        transformation = Matrix.init_identity ();
        font = {
          slant = Upright;
          weight = Normal;
          size = 10.;
          family = "sans-serif";
        };
        source = !(Pattern.create_rgb ~r:0. ~g:0. ~b:0.);
        fill_rule = WINDING;
      };
    ];
  } in
  set_line_width context 2.0;
  context

type operator = CLEAR | SOURCE | OVER | IN | OUT | ATOP | DEST | DEST_OVER | DEST_IN | DEST_OUT | DEST_ATOP | XOR | ADD | SATURATE

let set_operator context operator =
  let operator = match operator with
    | CLEAR -> failwith "Unsupported operator CLEAR"
    | SOURCE -> failwith "Unsupported operator SOURCE"
    | OVER -> "source-over"
    | ATOP -> "source-atop"
    | IN -> "source-in"
    | OUT -> "source-out"
    | DEST_OVER -> "destination-over"
    | DEST_ATOP -> "destination-atop"
    | DEST_IN -> "destination-in"
    | DEST_OUT -> "destination-out"
    | ADD -> "lighter"
    | XOR -> "xor"
    | DEST -> failwith "Unsupported operator DEST"
    | SATURATE -> failwith "Unsupported operator SATURATE"
  in
  context.html##.globalCompositeOperation := Js.string operator

let get_operator context =
  match Js.to_string context.html##.globalCompositeOperation with
    | "over" -> OVER (* Special case for node-canvas which seems to have a wrong default value *)
    | "add" -> ADD (* Special case for node-canvas *)
    | "source-over" -> OVER
    | "source-atop" -> ATOP
    | "source-in" -> IN
    | "source-out" -> OUT
    | "destination-over" -> DEST_OVER
    | "destination-atop" -> DEST_ATOP
    | "destination-in" -> DEST_IN
    | "destination-out" -> DEST_OUT
    | "lighter" -> ADD
    | "xor" -> XOR
    | op -> failwith (Printf.sprintf "Unexpected globalCompositeOperation %S" op)
