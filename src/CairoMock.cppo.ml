(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = sig
  #include "CairoMock.S.incl.mli"
end

module Mock = struct
  #include "Backend.incl.ml"

  module State = struct
    type t = {
      dashes: float array;
      fill_rule: fill_rule;
      font: font;
      line_cap: line_cap;
      line_join: line_join;
      line_width: float;
      miter_limit: float;
      ofs: float;
      operator: operator;
      source: Pattern.source;
      transformation: Matrix.t;
    }
  end

  type context = {
    mutable calls: string list;
    mutable states: State.t list;
    points: Points.t;
  }

  let create () = {
    calls = [];
    states = [
      {
        dashes = [||];
        fill_rule = WINDING;
        font = {
          slant = Upright;
          weight = Normal;
          size = 10.;
          family = "sans-serif";
        };
        line_cap = BUTT;
        line_join = JOIN_MITER;
        line_width = 2.;
        miter_limit = 10.;
        ofs = 0.;
        operator = OVER;
        source = !(Pattern.create_rgb ~r:0. ~g:0. ~b:0.);
        transformation = Matrix.init_identity ();
      };
    ];
    points = Points.create ();
  }

  let call context ret =
    Printf.ksprintf (fun call ->
      context.calls <- call::context.calls;
      ret
    )

  let state {states; _} =
    List.hd states

  let mutate_state context f format =
    let state = f (state context) in
    context.states <- state::(List.tl context.states);
    call context () format


  let save context =
    context.states <- (state context)::context.states;
    call context () "save"

  let restore context =
    let states =
      match context.states with
        | [] (*BISECT-IGNORE*) (* This cannot happen: restore is the only function removing states and it refuses to remove the last one. *)
        | [_] -> call context () "restore -> raise (Error INVALID_RESTORE)"; raise (Error INVALID_RESTORE)
        | _::states -> states
    in
    context.states <- states;
    call context () "restore"


  let print_matrix () {xx; xy; yx; yy; x0; y0} =
    Printf.sprintf "{xx=%.2f; xy=%.2f; yx=%.2f; yy=%.2f; x0=%.2f; y0=%.2f}" xx xy yx yy x0 y0

  let set_matrix context transformation =
    mutate_state context (fun s -> {s with transformation}) "set_matrix %a" print_matrix transformation

  let get_matrix context =
    let transformation = (state context).transformation in
    call context transformation "get_matrix -> %a" print_matrix transformation

  let transform_ m s =
    {s with State.transformation=Matrix.multiply s.State.transformation m}

  let transform context m =
    mutate_state context (transform_ m) "transform %a" print_matrix m

  let scale context ~x ~y =
    mutate_state context (transform_ (Matrix.init_scale ~x ~y)) "scale ~x:%.2f ~y:%.2f" x y

  let translate context ~x ~y =
    mutate_state context (transform_ (Matrix.init_translate ~x ~y)) "translate ~x:%.2f ~y:%.2f" x y

  let rotate context ~angle =
    mutate_state context (transform_ (Matrix.init_rotate ~angle)) "rotate ~angle:%.2f" angle

  let identity_matrix context =
    mutate_state context (fun s -> {s with transformation=Matrix.init_identity ()}) "identity_matrix"

  let device_to_user context ~x ~y =
    let (x', y') =
      Matrix.transform_point (Matrix.init_inverse (state context).transformation) ~x ~y
    in
    call context (x', y') "device_to_user ~x:%.2f ~y:%.2f -> (%.2f, %.2f)" x y x' y'

  let device_to_user_distance context ~x:dx ~y:dy =
    let (dx', dy') =
      Matrix.transform_distance (Matrix.init_inverse (state context).transformation) ~dx ~dy
    in
    call context (dx', dy') "device_to_user_distance ~x:%.2f ~y:%.2f -> (%.2f, %.2f)" dx dy dx' dy'

  let user_to_device context ~x ~y =
    let (x', y') =
      Matrix.transform_point (state context).transformation ~x ~y
    in
    call context (x', y') "user_to_device ~x:%.2f ~y:%.2f -> (%.2f, %.2f)" x y x' y'

  let user_to_device_distance context ~x:dx ~y:dy =
    let (dx', dy') =
      Matrix.transform_distance (state context).transformation ~dx ~dy
    in
    call context (dx', dy') "user_to_device_distance ~x:%.2f ~y:%.2f -> (%.2f, %.2f)" dx dy dx' dy'


  let mutate_points context ?(start=`None) ~current format =
    let transformation = (state context).transformation in
    let make_relative ~dx ~dy =
      let (x, y) =
        match Points.current context.points ~transformation with
          | None -> raise (Error NO_CURRENT_POINT)
          | Some (x, y) -> (x, y)
      in
      (x +. dx, y +. dy)
    in
    begin match start with
      | `None -> ()
      | `Reset -> Points.reset_start context.points
      | `IfNone (x, y) -> Points.set_start_if_none context.points ~transformation ~x ~y
      | `Set (x, y) -> Points.set_start context.points ~transformation ~x ~y
      | `Relative (dx, dy) ->
        let (x, y) = make_relative ~dx ~dy in
        Points.set_start context.points ~transformation ~x ~y
    end;
    begin match current with
      | `Reset -> Points.reset_current context.points
      | `FromStart -> Points.set_current_from_start context.points
      | `Set (x, y) -> Points.set_current context.points ~transformation ~x ~y
      | `Relative (dx, dy) ->
        let (x, y) = make_relative ~dx ~dy in
        Points.set_current context.points ~transformation ~x ~y
    end;
    call context () format


  let move_to context ~x ~y =
    mutate_points context ~start:(`Set (x, y)) ~current:`FromStart "move_to ~x:%.2f ~y:%.2f" x y

  let rel_move_to context ~x ~y =
    try
      mutate_points context ~start:(`Relative (x, y)) ~current:`FromStart "rel_move_to ~x:%.2f ~y:%.2f" x y
    with
      | (Error NO_CURRENT_POINT) as e -> begin
        call context () "rel_move_to ~x:%.2f ~y:%.2f -> raise (Error NO_CURRENT_POINT)" x y;
        raise e
      end

  let line_to context ~x ~y =
    mutate_points context ~start:(`IfNone (x, y)) ~current:(`Set (x, y)) "line_to ~x:%.2f ~y:%.2f" x y

  let rel_line_to context ~x ~y =
    try
      mutate_points context ~start:(`IfNone (x, y)) ~current:(`Relative (x, y)) "rel_line_to ~x:%.2f ~y:%.2f" x y
    with
      | (Error NO_CURRENT_POINT) as e -> begin
        call context () "rel_line_to ~x:%.2f ~y:%.2f -> raise (Error NO_CURRENT_POINT)" x y;
        raise e
      end

  let curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    mutate_points context ~start:(`IfNone (x1, y1)) ~current:(`Set (x3, y3)) "curve_to ~x1:%.2f ~y1:%.2f ~x2:%.2f ~y2:%.2f ~x3:%.2f ~y3:%.2f" x1 y1 x2 y2 x3 y3

  let rel_curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    try
      mutate_points context ~start:(`IfNone (x1, y1)) ~current:(`Relative (x3, y3)) "rel_curve_to ~x1:%.2f ~y1:%.2f ~x2:%.2f ~y2:%.2f ~x3:%.2f ~y3:%.2f" x1 y1 x2 y2 x3 y3
    with
      | (Error NO_CURRENT_POINT) as e -> begin
        call context () "rel_curve_to ~x1:%.2f ~y1:%.2f ~x2:%.2f ~y2:%.2f ~x3:%.2f ~y3:%.2f -> raise (Error NO_CURRENT_POINT)" x1 y1 x2 y2 x3 y3;
        raise e
      end

  let rectangle context ~x ~y ~w ~h =
    mutate_points context ~current:(`Set (x, y)) "rectangle ~x:%.2f ~y:%.2f ~w:%.2f ~h:%.2f" x y w h

  let arc context ~x ~y ~r ~a1 ~a2 =
    mutate_points context ~start:(`IfNone (x +. r *. (cos a1), y +. r *. (sin a1))) ~current:(`Set (x +. r *. (cos a2), y +. r *. (sin a2))) "arc ~x:%.2f ~y:%.2f ~r:%.2f ~a1:%.2f ~a2:%.2f" x y r a1 a2

  let arc_negative context ~x ~y ~r ~a1 ~a2 =
    mutate_points context ~start:(`IfNone (x +. r *. (cos a1), y +. r *. (sin a1))) ~current:(`Set (x +. r *. (cos a2), y +. r *. (sin a2))) "arc_negative ~x:%.2f ~y:%.2f ~r:%.2f ~a1:%.2f ~a2:%.2f" x y r a1 a2

  module Path = struct
    let get_current_point context =
      let transformation = (state context).transformation in
      let (x, y) =
        match Points.current ~transformation context.points with
          | None -> (0., 0.)
          | Some (x, y) -> (x, y)
      in
      call context (x, y) "Path.get_current_point -> (%.2f, %.2f)" x y

    let clear context =
      mutate_points context ~start:`Reset ~current:`Reset "Path.clear"

    let close context =
      mutate_points context ~current:`FromStart "Path.close"
  end

  let stroke_preserve context =
    call context () "stroke_preserve"

  let stroke context =
    mutate_points context ~start:`Reset ~current:`Reset "stroke"

  let fill_preserve context =
    call context () "fill_preserve"

  let fill context =
    mutate_points context ~start:`Reset ~current:`Reset "fill"

  let clip_preserve context =
    call context () "clip_preserve"

  let clip context =
    mutate_points context ~start:`Reset ~current:`Reset "clip"

  let paint ?(alpha=1.) context =
    call context () "paint ~alpha:%.2f" alpha


  let set_line_width context line_width =
    mutate_state context (fun s -> {s with line_width}) "set_line_width %.2f" line_width

  let get_line_width context =
    let line_width = (state context).line_width in
    call context line_width "get_line_width -> %.2f" line_width

  let print_dashes () dashes =
    dashes
    |> Array.to_list
    |> List.map ~f:(Printf.sprintf "%.2f")
    |> String.concat ~sep:"; "
    |> Printf.sprintf "[|%s|]"

  let set_dash context ?(ofs=0.) dashes =
    mutate_state context (fun s -> {s with dashes; ofs}) "set_dash ~ofs:%.2f %a" ofs print_dashes dashes

  let get_dash context =
    let state = state context in
    call context (state.dashes, state.ofs) "get_dash -> (%a, %.2f)" print_dashes state.dashes state.ofs

  let print_fill_rule () = function
    | WINDING -> "WINDING"
    | EVEN_ODD -> "EVEN_ODD"

  let set_fill_rule context fill_rule =
    mutate_state context (fun s -> {s with fill_rule}) "set_fill_rule %a" print_fill_rule fill_rule

  let get_fill_rule context =
    let fill_rule = (state context).fill_rule in
    call context fill_rule "get_fill_rule -> %a" print_fill_rule fill_rule

  let print_line_cap () = function
    | BUTT -> "BUTT"
    | ROUND -> "ROUND"
    | SQUARE -> "SQUARE"

  let set_line_cap context line_cap =
    mutate_state context (fun s -> {s with line_cap}) "set_line_cap %a" print_line_cap line_cap

  let get_line_cap context =
    let line_cap = (state context).line_cap in
    call context line_cap "get_line_cap -> %a" print_line_cap line_cap

  let print_line_join () = function
    | JOIN_MITER -> "JOIN_MITER"
    | JOIN_ROUND -> "JOIN_ROUND"
    | JOIN_BEVEL -> "JOIN_BEVEL"

  let set_line_join context line_join =
    mutate_state context (fun s -> {s with line_join}) "set_line_join %a" print_line_join line_join

  let get_line_join context =
    let line_join = (state context).line_join in
    call context line_join "get_line_join -> %a" print_line_join line_join

  let set_miter_limit context miter_limit =
    mutate_state context (fun s -> {s with miter_limit}) "set_miter_limit %.2f" miter_limit

  let get_miter_limit context =
    let miter_limit = (state context).miter_limit in
    call context miter_limit "get_miter_limit -> %.2f" miter_limit

  let print_operator () = function
    | CLEAR -> "CLEAR"
    | SOURCE -> "SOURCE"
    | OVER -> "OVER"
    | IN -> "IN"
    | OUT -> "OUT"
    | ATOP -> "ATOP"
    | DEST -> "DEST"
    | DEST_OVER -> "DEST_OVER"
    | DEST_IN -> "DEST_IN"
    | DEST_OUT -> "DEST_OUT"
    | DEST_ATOP -> "DEST_ATOP"
    | XOR -> "XOR"
    | ADD -> "ADD"
    | SATURATE -> "SATURATE"

  let set_operator context operator =
    mutate_state context (fun s -> {s with operator}) "set_operator %a" print_operator operator

  let get_operator context =
    let operator = (state context).operator in
    call context operator "get_operator -> %a" print_operator operator


  let print_stop_point_list () ps =
    ps
    |> Pattern.StopPointList.to_list
    |> List.map ~f:(fun (position, r, g, b, a) -> Printf.sprintf "{position=%.2f; r=%.2f; g=%.2f; b=%.2f; a=%.2f}" position r g b a)
    |> String.concat ~sep:"; "
    |> Printf.sprintf "[%s]"

  let print_source () = function
    | Pattern.Rgba (r, g, b, a) ->
      Printf.sprintf "Rgba {r=%.2f; g=%.2f; b=%.2f; a=%.2f}" r g b a
    | Pattern.LinearGradient {points=(x1, y1, x2, y2); stop_points} ->
      Printf.sprintf "LinearGradient {x0=%.2f; y0=%.2f; x1=%.2f; y1=%.2f; stop_points=%a}" x1 y1 x2 y2 print_stop_point_list stop_points
    | Pattern.RadialGradient {circles=(x1, y1, r1, x2, y2, r2); stop_points} ->
      Printf.sprintf "RadialGradient {x0=%.2f; y0=%.2f; r0=%.2f; x1=%.2f; y1=%.2f; r1%.2f; stop_points=%a}" x1 y1 r1 x2 y2 r2 print_stop_point_list stop_points
    | Pattern.TypeMismatch -> "TypeMismatch" (*BISECT-IGNORE*) (* Impossible case, or.. Am I being lazy? *)

  let set_source context pattern =
    let source = !pattern in
    mutate_state context (fun s -> {s with source}) "set_source (%a)" print_source source

  let get_source context =
    let source = (state context).source in
    call context (ref source) "get_source -> (%a)" print_source source

  let set_source_rgb context ~r ~g ~b =
    let source = !(Pattern.create_rgb ~r ~g ~b) in
    mutate_state context (fun s -> {s with source}) "set_source_rgb ~r:%.2f ~g:%.2f ~b:%.2f" r g b

  let set_source_rgba context ~r ~g ~b ~a =
    let source = !(Pattern.create_rgba ~r ~g ~b ~a) in
    mutate_state context (fun s -> {s with source}) "set_source_rgba ~r:%.2f ~g:%.2f ~b:%.2f ~a:%.2f" r g b a


  let print_slant () = function
    | Upright -> "Upright"
    | Italic -> "Italic"
    | Oblique -> "Oblique"

  let print_weight () = function
    | Normal -> "Normal"
    | Bold -> "Bold"

  let select_font_face context ?(slant=Upright) ?(weight=Normal) family =
    mutate_state context (fun s -> {s with font={s.font with slant; weight; family}}) "select_font_face ~slant:%a ~weight:%a %S" print_slant slant print_weight weight family

  let set_font_size context size =
    mutate_state context (fun s -> {s with font={s.font with size}}) "set_font_size %.2f" size

  let show_text context s =
    call context () "show_text %S" s

  let print_font_extents () {ascent; descent; baseline; max_x_advance; max_y_advance} =
    Printf.sprintf "{ascent=%.2f; descent=%.2f; baseline=%.2f; max_x_advance=%.2f; max_y_advance=%.2f}" ascent descent baseline max_x_advance max_y_advance

  let font_extents context =
    let ascent = (state context).font.size in
    let extents = {ascent; descent=ascent /. 4.; baseline=0.; max_x_advance=2. *. ascent; max_y_advance=0.} in
    call context extents "font_extents -> %a" print_font_extents extents

  let print_text_extents () {x_bearing; y_bearing; width; height; x_advance; y_advance} =
    Printf.sprintf "{x_bearing=%.2f; y_bearing=%.2f; width=%.2f; height=%.2f; x_advance=%.2f; y_advance=%.2f}" x_bearing y_bearing width height x_advance y_advance

  let text_extents context s =
    let width =
      (state context).font.size *. 0.8 *. (float_of_int (String.length s))
    and height =
      (state context).font.size
    in
    let extents = {x_bearing=0.; y_bearing=0.; width; height; x_advance=width; y_advance=0.} in
    call context extents "text_extents %S -> %a" s print_text_extents extents
end

module Decorate(C: S) = struct
  open StdLabels

  type fill_rule = C.fill_rule =
    | WINDING
    | EVEN_ODD

  type line_cap = C.line_cap =
    | BUTT
    | ROUND
    | SQUARE

  type line_join = C.line_join =
    | JOIN_MITER
    | JOIN_ROUND
    | JOIN_BEVEL

  type matrix = C.matrix= {
    mutable xx: float;
    mutable yx: float;
    mutable xy: float;
    mutable yy: float;
    mutable x0: float;
    mutable y0: float;
  }

  type text_extents = C.text_extents = {
    x_bearing : float;
    y_bearing : float;
    width : float;
    height : float;
    x_advance : float;
    y_advance : float;
  }

  type font_extents = C.font_extents = {
    ascent: float;
    descent: float;
    baseline: float;
    max_x_advance: float;
    max_y_advance: float;
  }

  type operator = C.operator =
    | CLEAR
    | SOURCE
    | OVER
    | IN
    | OUT
    | ATOP
    | DEST
    | DEST_OVER
    | DEST_IN
    | DEST_OUT
    | DEST_ATOP
    | XOR
    | ADD
    | SATURATE

  type weight = C.weight =
    | Normal
    | Bold

  type slant = C.slant =
    | Upright
    | Italic
    | Oblique

  type status = C.status =
    | INVALID_RESTORE
    | INVALID_POP_GROUP
    | NO_CURRENT_POINT
    | INVALID_MATRIX
    | INVALID_STATUS
    | NULL_POINTER
    | INVALID_STRING
    | INVALID_PATH_DATA
    | READ_ERROR
    | WRITE_ERROR
    | SURFACE_FINISHED
    | SURFACE_TYPE_MISMATCH
    | PATTERN_TYPE_MISMATCH
    | INVALID_CONTENT
    | INVALID_FORMAT
    | INVALID_VISUAL
    | FILE_NOT_FOUND
    | INVALID_DASH
    | INVALID_DSC_COMMENT
    | INVALID_INDEX
    | CLIP_NOT_REPRESENTABLE
    | TEMP_FILE_ERROR
    | INVALID_STRIDE
    | FONT_TYPE_MISMATCH
    | USER_FONT_IMMUTABLE
    | USER_FONT_ERROR
    | NEGATIVE_COUNT
    | INVALID_CLUSTERS
    | INVALID_SLANT
    | INVALID_WEIGHT
    | INVALID_SIZE
    | USER_FONT_NOT_IMPLEMENTED
    | DEVICE_TYPE_MISMATCH
    | DEVICE_ERROR
    | INVALID_MESH_CONSTRUCTION
    | DEVICE_FINISHED
    | JBIG2_GLOBAL_MISSING

  module Pattern = C.Pattern

  module Matrix = C.Matrix

  exception Error = C.Error

  exception Unavailable = C.Unavailable

  let status_to_string = C.status_to_string

  module P = struct
    open C

    let unit () =
      ""

    let float x =
      Printf.sprintf "%.2f" x

    let status = function
      | INVALID_RESTORE -> "INVALID_RESTORE"
      | NO_CURRENT_POINT -> "NO_CURRENT_POINT"
      (*BISECT-IGNORE-BEGIN*) (* @todo Can we make Cairo raise some of these exceptions? *)
      | INVALID_POP_GROUP -> "INVALID_POP_GROUP"
      | INVALID_MATRIX -> "INVALID_MATRIX"
      | INVALID_STATUS -> "INVALID_STATUS"
      | NULL_POINTER -> "NULL_POINTER"
      | INVALID_STRING -> "INVALID_STRING"
      | INVALID_PATH_DATA -> "INVALID_PATH_DATA"
      | READ_ERROR -> "READ_ERROR"
      | WRITE_ERROR -> "WRITE_ERROR"
      | SURFACE_FINISHED -> "SURFACE_FINISHED"
      | SURFACE_TYPE_MISMATCH -> "SURFACE_TYPE_MISMATCH"
      | PATTERN_TYPE_MISMATCH -> "PATTERN_TYPE_MISMATCH"
      | INVALID_CONTENT -> "INVALID_CONTENT"
      | INVALID_FORMAT -> "INVALID_FORMAT"
      | INVALID_VISUAL -> "INVALID_VISUAL"
      | FILE_NOT_FOUND -> "FILE_NOT_FOUND"
      | INVALID_DASH -> "INVALID_DASH"
      | INVALID_DSC_COMMENT -> "INVALID_DSC_COMMENT"
      | INVALID_INDEX -> "INVALID_INDEX"
      | CLIP_NOT_REPRESENTABLE -> "CLIP_NOT_REPRESENTABLE"
      | TEMP_FILE_ERROR -> "TEMP_FILE_ERROR"
      | INVALID_STRIDE -> "INVALID_STRIDE"
      | FONT_TYPE_MISMATCH -> "FONT_TYPE_MISMATCH"
      | USER_FONT_IMMUTABLE -> "USER_FONT_IMMUTABLE"
      | USER_FONT_ERROR -> "USER_FONT_ERROR"
      | NEGATIVE_COUNT -> "NEGATIVE_COUNT"
      | INVALID_CLUSTERS -> "INVALID_CLUSTERS"
      | INVALID_SLANT -> "INVALID_SLANT"
      | INVALID_WEIGHT -> "INVALID_WEIGHT"
      | INVALID_SIZE -> "INVALID_SIZE"
      | USER_FONT_NOT_IMPLEMENTED -> "USER_FONT_NOT_IMPLEMENTED"
      | DEVICE_TYPE_MISMATCH -> "DEVICE_TYPE_MISMATCH"
      | DEVICE_ERROR -> "DEVICE_ERROR"
      | INVALID_MESH_CONSTRUCTION -> "INVALID_MESH_CONSTRUCTION"
      | DEVICE_FINISHED -> "DEVICE_FINISHED"
      | JBIG2_GLOBAL_MISSING -> "JBIG2_GLOBAL_MISSING"
      (*BISECT-IGNORE-END*)

    let matrix {xx; xy; yx; yy; x0; y0} =
      Printf.sprintf "{xx=%.2f; xy=%.2f; yx=%.2f; yy=%.2f; x0=%.2f; y0=%.2f}" xx xy yx yy x0 y0

    let coords (x, y) =
      Printf.sprintf "(%.2f, %.2f)" x y

    let dashes_ofs (dashes, ofs) =
      let dashes =
        dashes
        |> Array.to_list
        |> List.map ~f:(Printf.sprintf "%.2f")
        |> String.concat ~sep:"; "
      in
      Printf.sprintf "([|%s|], %.2f)" dashes ofs

    let fill_rule = function
      | WINDING -> "WINDING"
      | EVEN_ODD -> "EVEN_ODD"

    let line_cap = function
      | BUTT -> "BUTT"
      | ROUND -> "ROUND"
      | SQUARE -> "SQUARE"

    let line_join = function
      | JOIN_MITER -> "JOIN_MITER"
      | JOIN_ROUND -> "JOIN_ROUND"
      | JOIN_BEVEL -> "JOIN_BEVEL"

    let operator = function
      | CLEAR -> "CLEAR"
      | SOURCE -> "SOURCE"
      | OVER -> "OVER"
      | IN -> "IN"
      | OUT -> "OUT"
      | ATOP -> "ATOP"
      | DEST -> "DEST"
      | DEST_OVER -> "DEST_OVER"
      | DEST_IN -> "DEST_IN"
      | DEST_OUT -> "DEST_OUT"
      | DEST_ATOP -> "DEST_ATOP"
      | XOR -> "XOR"
      | ADD -> "ADD"
      | SATURATE -> "SATURATE"

    let source source = Pattern.(
      let get get =
        try Some (get source) with Error PATTERN_TYPE_MISMATCH -> None
      and stop_points () =
        let count = get_color_stop_count source in
        let stop_points = Array.make count (0., 0., 0., 0., 0.) in
        for idx = 0 to count - 1 do
          stop_points.(idx) <- get_color_stop_rgba source ~idx
        done;
        stop_points
        |> Array.to_list
        |> List.map ~f:(fun (position, r, g, b, a) -> Printf.sprintf "{position=%.2f; r=%.2f; g=%.2f; b=%.2f; a=%.2f}" position r g b a)
        |> String.concat ~sep:"; "
        |> Printf.sprintf "[%s]"
      in
      match get get_rgba with
        | Some (r, g, b, a) ->
          Printf.sprintf "(Rgba {r=%.2f; g=%.2f; b=%.2f; a=%.2f})" r g b a
        | None -> match get get_linear_points with
          | Some (x1, y1, x2, y2) ->
            Printf.sprintf "(LinearGradient {x0=%.2f; y0=%.2f; x1=%.2f; y1=%.2f; stop_points=%s})" x1 y1 x2 y2 (stop_points ())
          | None ->
            let (x1, y1, r1, x2, y2, r2) = get_radial_circles source in
            Printf.sprintf "(RadialGradient {x0=%.2f; y0=%.2f; r0=%.2f; x1=%.2f; y1=%.2f; r1%.2f; stop_points=%s})" x1 y1 r1 x2 y2 r2 (stop_points ())
    )

    let font_extents {ascent; descent; baseline; max_x_advance; max_y_advance} =
      Printf.sprintf "{ascent=%.2f; descent=%.2f; baseline=%.2f; max_x_advance=%.2f; max_y_advance=%.2f}" ascent descent baseline max_x_advance max_y_advance

    let text_extents {x_bearing; y_bearing; width; height; x_advance; y_advance} =
      Printf.sprintf "{x_bearing=%.2f; y_bearing=%.2f; width=%.2f; height=%.2f; x_advance=%.2f; y_advance=%.2f}" x_bearing y_bearing width height x_advance y_advance

    let slant = function
      | Upright -> "Upright"
      | Italic -> "Italic"
      | Oblique -> "Oblique"

    let weight = function
      | Normal -> "Normal"
      | Bold -> "Bold"
  end

  module A = struct
    let matrix () =
      P.matrix

    let operator () =
      P.operator

    let fill_rule () =
      P.fill_rule

    let line_cap () =
      P.line_cap

    let line_join () =
      P.line_join

    let source () =
      P.source

    let dashes () dashes =
      (* @todo Factorize with P.dashes_ofs *)
      dashes
      |> Array.to_list
      |> List.map ~f:(Printf.sprintf "%.2f")
      |> String.concat ~sep:"; "
      |> Printf.sprintf "[|%s|]"

    let option name p () = function
      | None -> ""
      | Some x -> Printf.sprintf " ~%s:%s" name (p x)
  end

  module Context: sig
    type t

    val create: C.context -> t
    val call: t -> ('a, unit, string, ('b -> string) -> (C.context -> 'b) -> 'b) format4 -> 'a
    val calls: t -> string list
  end = struct
    type t = {
      c: C.context;
      mutable calls: string list;
    }

    let create c =
      {c; calls=[]}

    let calls {calls; _} =
      List.rev calls

    let call context format =
      Printf.ksprintf (fun call print_ret f ->
        try begin
          let ret = f context.c in
          let printed_ret = print_ret ret in
          let call =
            if printed_ret <> "" then
              Printf.sprintf "%s -> %s" call printed_ret
            else
              call
          in
          context.calls <- call::context.calls;
          ret
        end with
          | (C.Error status) as ex -> begin
            (* @todo Be more generic: print and re-raise any exception *)
            let call = Printf.sprintf "%s -> raise (Error %s)" call (P.status status) in
            context.calls <- call::context.calls;
            raise ex
          end
      ) format
  end

  type context = Context.t

  let calls = Context.calls

  #define CC Context.call context


  let save context =
    CC "save" P.unit C.save

  let restore context =
    CC "restore" P.unit C.restore


  let set_matrix context m =
    CC "set_matrix %a" A.matrix m P.unit (fun c -> C.set_matrix c m)

  let get_matrix context =
    CC "get_matrix" P.matrix C.get_matrix

  let transform context m =
    CC "transform %a" A.matrix m P.unit (fun c -> C.transform c m)

  let scale context ~x ~y =
    CC "scale ~x:%.2f ~y:%.2f" x y P.unit (C.scale ~x ~y)

  let translate context ~x ~y =
    CC "translate ~x:%.2f ~y:%.2f" x y P.unit (C.translate ~x ~y)

  let rotate context ~angle =
    CC "rotate ~angle:%.2f" angle P.unit (C.rotate ~angle)

  let identity_matrix context =
    CC "identity_matrix" P.unit C.identity_matrix

  let device_to_user context ~x ~y =
    CC "device_to_user ~x:%.2f ~y:%.2f" x y P.coords (C.device_to_user ~x ~y)

  let device_to_user_distance context ~x ~y =
    CC "device_to_user_distance ~x:%.2f ~y:%.2f" x y P.coords (C.device_to_user_distance ~x ~y)

  let user_to_device context ~x ~y =
    CC "user_to_device ~x:%.2f ~y:%.2f" x y P.coords (C.user_to_device ~x ~y)

  let user_to_device_distance context ~x ~y =
    CC "user_to_device_distance ~x:%.2f ~y:%.2f" x y P.coords (C.user_to_device_distance ~x ~y)


  let move_to context ~x ~y =
    CC "move_to ~x:%.2f ~y:%.2f" x y P.unit (C.move_to ~x ~y)

  let rel_move_to context ~x ~y =
    CC "rel_move_to ~x:%.2f ~y:%.2f" x y P.unit (C.rel_move_to ~x ~y)

  let line_to context ~x ~y =
    CC "line_to ~x:%.2f ~y:%.2f" x y P.unit (C.line_to ~x ~y)

  let rel_line_to context ~x ~y =
    CC "rel_line_to ~x:%.2f ~y:%.2f" x y P.unit (C.rel_line_to ~x ~y)

  let curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    CC "curve_to ~x1:%.2f ~y1:%.2f ~x2:%.2f ~y2:%.2f ~x3:%.2f ~y3:%.2f" x1 y1 x2 y2 x3 y3 P.unit (C.curve_to ~x1 ~y1 ~x2 ~y2 ~x3 ~y3)

  let rel_curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    CC "rel_curve_to ~x1:%.2f ~y1:%.2f ~x2:%.2f ~y2:%.2f ~x3:%.2f ~y3:%.2f" x1 y1 x2 y2 x3 y3 P.unit (C.rel_curve_to ~x1 ~y1 ~x2 ~y2 ~x3 ~y3)

  let rectangle context ~x ~y ~w ~h =
    CC "rectangle ~x:%.2f ~y:%.2f ~w:%.2f ~h:%.2f" x y w h P.unit (C.rectangle ~x ~y ~w ~h)

  let arc context ~x ~y ~r ~a1 ~a2 =
    CC "arc ~x:%.2f ~y:%.2f ~r:%.2f ~a1:%.2f ~a2:%.2f" x y r a1 a2 P.unit (C.arc ~x ~y ~r ~a1 ~a2)

  let arc_negative context ~x ~y ~r ~a1 ~a2 =
    CC "arc_negative ~x:%.2f ~y:%.2f ~r:%.2f ~a1:%.2f ~a2:%.2f" x y r a1 a2 P.unit (C.arc_negative ~x ~y ~r ~a1 ~a2)

  module Path = struct
    let get_current_point context =
      CC "Path.get_current_point" P.coords C.Path.get_current_point

    let clear context =
      CC "Path.clear" P.unit C.Path.clear

    let close context =
      CC "Path.close" P.unit C.Path.close
  end

  let stroke_preserve context =
    CC "stroke_preserve" P.unit C.stroke_preserve

  let stroke context =
    CC "stroke" P.unit C.stroke

  let fill_preserve context =
    CC "fill_preserve" P.unit C.fill_preserve

  let fill context =
    CC "fill" P.unit C.fill

  let clip_preserve context =
    CC "clip_preserve" P.unit C.clip_preserve

  let clip context =
    CC "clip" P.unit C.clip

  let paint ?alpha context =
    CC "paint%a" (A.option "alpha" P.float) alpha P.unit (C.paint ?alpha)


  let set_line_width context line_width =
    CC "set_line_width %.2f" line_width P.unit (fun c -> C.set_line_width c line_width)

  let get_line_width context =
    CC "get_line_width" P.float C.get_line_width

  let set_dash context ?ofs dashes =
    CC "set_dash%a %a" (A.option "ofs" P.float) ofs A.dashes dashes P.unit (fun c -> C.set_dash c ?ofs dashes)

  let get_dash context =
    CC "get_dash" P.dashes_ofs C.get_dash

  let set_fill_rule context fill_rule =
    CC "set_fill_rule %a" A.fill_rule fill_rule P.unit (fun c -> C.set_fill_rule c fill_rule)

  let get_fill_rule context =
    CC "get_fill_rule" P.fill_rule C.get_fill_rule

  let set_line_cap context line_cap =
    CC "set_line_cap %a" A.line_cap line_cap P.unit (fun c -> C.set_line_cap c line_cap)

  let get_line_cap context =
    CC "get_line_cap" P.line_cap C.get_line_cap

  let set_line_join context line_join =
    CC "set_line_join %a" A.line_join line_join P.unit (fun c -> C.set_line_join c line_join)

  let get_line_join context =
    CC "get_line_join" P.line_join C.get_line_join

  let set_miter_limit context miter_limit =
    CC "set_miter_limit %.2f" miter_limit P.unit (fun c -> C.set_miter_limit c miter_limit)

  let get_miter_limit context =
    CC "get_miter_limit" P.float C.get_miter_limit

  let set_operator context operator =
    CC "set_operator %a" A.operator operator P.unit (fun c -> C.set_operator c operator)

  let get_operator context =
    CC "get_operator" P.operator C.get_operator


  let set_source context source =
    CC "set_source %a" A.source source P.unit (fun c -> C.set_source c source)

  let get_source context =
    CC "get_source" P.source C.get_source

  let set_source_rgb context ~r ~g ~b =
    CC "set_source_rgb ~r:%.2f ~g:%.2f ~b:%.2f" r g b P.unit (C.set_source_rgb ~r ~g ~b)

  let set_source_rgba context ~r ~g ~b ~a =
    CC "set_source_rgba ~r:%.2f ~g:%.2f ~b:%.2f ~a:%.2f" r g b a P.unit (C.set_source_rgba ~r ~g ~b ~a)


  let select_font_face context ?slant ?weight family =
    CC "select_font_face%a%a %S" (A.option "slant" P.slant) slant (A.option "weight" P.weight) weight family P.unit (fun c -> C.select_font_face c ?slant ?weight family)

  let set_font_size context font_size =
    CC "set_font_size %.2f" font_size P.unit (fun c -> C.set_font_size c font_size)

  let show_text context s =
    CC "show_text %S" s P.unit (fun c -> C.show_text c s)

  let font_extents context =
    CC "font_extents" P.font_extents C.font_extents

  let text_extents context s =
    CC "text_extents %S" s P.text_extents (fun c -> C.text_extents c s)
end

include Decorate(Mock)

let create () =
  Context.create (Mock.create ())
