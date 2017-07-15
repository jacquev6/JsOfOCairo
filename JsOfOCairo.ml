(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
module Printf = OCamlStandard.Printf

module type S = module type of JsOfOCairo_S

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

type slant = Upright | Italic | Oblique

type weight = Normal | Bold

type _font = {
  slant: slant;
  weight: weight;
  size: float;
  family: string;
}

module Pattern = struct
  type 'a t = {
    kind: 'a;
    r: float;
    g: float;
    b: float;
    a: float;
  } constraint 'a = [<`Solid | `Surface | `Gradient | `Linear | `Radial]

  type any = [`Solid | `Surface | `Gradient | `Linear | `Radial] t

  let create_rgb ~r ~g ~b =
    {kind=`Solid; r; g; b; a=1.}

  let create_rgba ~r ~g ~b ~a =
    {kind=`Solid; r; g; b; a}

  let get_rgba {kind; r; g; b; a} =
    match kind with
      | `Solid -> (r, g, b, a)
      | (`Surface|`Gradient|`Linear|`Radial) -> raise (Error PATTERN_TYPE_MISMATCH)
end

type context = {
  ctx: Dom_html.canvasRenderingContext2D Js.t;
  mutable start_point: (float * float) option;
  mutable current_point: (float * float) option;
  mutable transformation: Matrix.t;
  mutable saved_transformations: Matrix.t list;
  mutable font: _font;
  mutable source: Pattern.any;
}

let set_line_width context width =
  context.ctx##.lineWidth := width

let get_line_width context =
  context.ctx##.lineWidth

module Path = struct
  let get_current_point {current_point; _} =
    current_point
    |> Opt.value_def ~def:(0., 0.)

  let clear context =
    context.ctx##beginPath;
    context.current_point <- None

  let close context =
    context.ctx##closePath;
    context.current_point <- context.start_point
end

let move_to context ~x ~y =
  context.ctx##moveTo x y;
  context.start_point <- Some (x, y);
  context.current_point <- context.start_point

let line_to context ~x ~y =
  context.ctx##lineTo x y;
  context.current_point <- Some (x, y)

let arc context ~x ~y ~r ~a1 ~a2 =
  context.ctx##arc x y r a1 a2 Js._false;
  if Opt.is_none context.start_point then
  context.start_point <- Some (x +. r *. (Math.cos a1), y +. r *. (Math.sin a1));
  context.current_point <- Some (x +. r *. (Math.cos a2), y +. r *. (Math.sin a2))

let arc_negative context ~x ~y ~r ~a1 ~a2 =
  context.ctx##arc x y r a1 a2 Js._true;
  if Opt.is_none context.start_point then
  context.start_point <- Some (x +. r *. (Math.cos a1), y +. r *. (Math.sin a1));
  context.current_point <- Some (x +. r *. (Math.cos a2), y +. r *. (Math.sin a2))

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

let set_source context ({Pattern.kind; r; g; b; a} as pattern) =
  let convert x = Int.to_string (Int.of_float (255.0 *. x)) in
  begin
    match kind with
      | `Solid -> begin
        let color = Js.string (Printf.sprintf "rgba(%s, %s, %s, %f)" (convert r) (convert g) (convert b) a) in
        context.ctx##.fillStyle := color;
        context.ctx##.strokeStyle := color
      end
      | `Surface -> failwith "Unsupported pattern `Surface"
      | `Gradient -> failwith "Unsupported pattern `Gradient"
      | `Linear -> failwith "Unsupported pattern `Linear"
      | `Radial -> failwith "Unsupported pattern `Radial"
  end;
  context.source <- pattern

let get_source {source; _} =
  source

let set_source_rgb context ~r ~g ~b =
  set_source context (Pattern.create_rgb ~r ~g ~b)

let set_source_rgba context ~r ~g ~b ~a =
  set_source context (Pattern.create_rgba ~r ~g ~b ~a)

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
    |> Opt.map ~f:(Matrix.transform_point' context.transformation)
    |> Opt.map ~f:(Matrix.rev_transform_point' m);
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
  context.current_point <- Some (x3, y3);
  context.ctx##bezierCurveTo x1 y1 x2 y2 x3 y3

let rel_curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  let (x1, y1) = make_rel context ~x:x1 ~y:y1
  and (x2, y2) = make_rel context ~x:x2 ~y:y2
  and (x3, y3) = make_rel context ~x:x3 ~y:y3 in
  curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

let rectangle context ~x ~y ~w ~h =
  context.current_point <- Some (x, y);
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

let _set_font context ({slant; weight; size; family} as font) =
  context.font <- font;
  let font_style = match slant with
    | Upright -> "normal"
    | Italic -> "italic"
    | Oblique -> "oblique"
  and font_weight = match weight with
    | Normal -> "normal"
    | Bold -> "bold"
  in
  let font = Printf.sprintf "%s %s %npx %s" font_style font_weight (Int.of_float size) family in
  context.ctx##.font := Js.string font

let _get_font ctx =
  (* @todo Test performance. This looks costly in DrawGrammar: we change fonts a lot, for each Token, Terminal, and NonTerminal.
  We could cache this data in the context, and invalidate the cache on C.restore *)
  ctx##.font
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

let restore context =
  match context.saved_transformations with
    | [] -> raise (Error INVALID_RESTORE)
    | matrix::saved_transformations -> begin
      context.ctx##restore;
      set_matrix context matrix;
      context.saved_transformations <- saved_transformations;
      context.font <- _get_font context.ctx
    end

let select_font_face context ?(slant=Upright) ?(weight=Normal) family =
  _set_font context {context.font with slant; weight; family}

let set_font_size context size =
  _set_font context {context.font with size}

let show_text context s =
  let (x, y) = Path.get_current_point context in
  context.ctx##fillText (Js.string s) x y

let font_extents context =
  let {size; _} = context.font in
  {
    ascent = size;
    descent = size /. 4.;
    baseline = 0.;
    max_x_advance = 2. *. size;
    max_y_advance = 0.;
  }

let text_extents context s =
  let {size; _} = context.font
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

let create canvas =
  let ctx = canvas##getContext Dom_html._2d_ in
  let context = {
    ctx;
    start_point = None;
    current_point = None;
    transformation = Matrix.init_identity ();
    saved_transformations = [];
    font = _get_font ctx;
    source = Pattern.create_rgb ~r:0. ~g:0. ~b:0.;
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
  context.ctx##.globalCompositeOperation := Js.string operator

let get_operator context =
  match Js.to_string context.ctx##.globalCompositeOperation with
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
