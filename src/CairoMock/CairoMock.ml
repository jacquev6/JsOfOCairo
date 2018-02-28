open StdLabels

module Backend = Backend

include Backend

module State = struct
  type t = {
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
      transformation = Matrix.init_identity ();
    };
  ];
  points = Points.create ();
}

let call context =
  Printf.ksprintf (fun call ->
    context.calls <- call::context.calls
  )

let calls {calls; _} =
  List.rev calls

let state {states; _} =
  List.hd states

let set_line_width context width =
  failwith "Not yet implemented: set_line_width"

let get_line_width context =
  failwith "Not yet implemented: get_line_width"

let set_dash context ?(ofs=0.) dashes =
  failwith "Not yet implemented: set_dash"

let get_dash context =
  failwith "Not yet implemented: get_dash"

let move_to context ~x ~y =
  call context "move_to ~x:%.2f ~y:%.2f" x y;
  let transformation = (state context).transformation in
  Points.set_start context.points ~transformation ~x ~y;
  Points.set_current_from_start context.points

let line_to context ~x ~y =
  call context "line_to ~x:%.2f ~y:%.2f" x y;
  let transformation = (state context).transformation in
  Points.set_start_if_none context.points ~transformation ~x ~y;
  Points.set_current context.points ~transformation ~x ~y

let arc context ~x ~y ~r ~a1 ~a2 =
  failwith "Not yet implemented: arc"

let arc_negative context ~x ~y ~r ~a1 ~a2 =
  failwith "Not yet implemented: arc_negative"

let set_source context pattern =
  failwith "Not yet implemented: set_source"

let get_source context =
  failwith "Not yet implemented: get_source"

let set_source_rgb context ~r ~g ~b =
  failwith "Not yet implemented: set_source_rgb"

let set_source_rgba context ~r ~g ~b ~a =
  failwith "Not yet implemented: set_source_rgba"

let device_to_user context ~x ~y =
  failwith "Not yet implemented: device_to_user"

let device_to_user_distance context ~x ~y =
  failwith "Not yet implemented: device_to_user_distance"

let user_to_device context ~x ~y =
  failwith "Not yet implemented: user_to_device"

let user_to_device_distance context ~x ~y =
  failwith "Not yet implemented: user_to_device_distance"

module Path = struct
  let get_current_point context =
    failwith "Not yet implemented: Path.get_current_point"

  let clear_ context =
    Points.reset_start context.points;
    Points.reset_current context.points

  let clear context =
    failwith "Not yet implemented: Path.clear"

  let close context =
    failwith "Not yet implemented: Path.close"
end

let stroke_preserve context =
  failwith "Not yet implemented: stroke_preserve"

let stroke context =
  call context "stroke";
  Path.clear_ context

let set_fill_rule context fill_rule =
  failwith "Not yet implemented: set_fill_rule"

let get_fill_rule context =
  failwith "Not yet implemented: get_fill_rule"

let fill_preserve context =
  failwith "Not yet implemented: fill_preserve"

let fill context =
  failwith "Not yet implemented: fill"

let clip_preserve context =
  failwith "Not yet implemented: clip_preserve"

let clip context =
  failwith "Not yet implemented: clip"

let set_matrix context ({xx; xy; yx; yy; x0; y0} as transformation) =
  failwith "Not yet implemented: set_matrix"

let get_matrix context =
  failwith "Not yet implemented: get_matrix"

let transform context m =
  failwith "Not yet implemented: transform"

let scale context ~x ~y =
  failwith "Not yet implemented: scale"

let translate context ~x ~y =
  failwith "Not yet implemented: translate"

let rotate context ~angle =
  failwith "Not yet implemented: rotate"

let identity_matrix context =
  failwith "Not yet implemented: identity_matrix"

let save context =
  failwith "Not yet implemented: save"

let restore context =
  failwith "Not yet implemented: restore"

let set_line_cap context cap =
  failwith "Not yet implemented: set_line_cap"

let get_line_cap context =
  failwith "Not yet implemented: get_line_cap"

let set_line_join context join =
  failwith "Not yet implemented: set_line_join"

let get_line_join context =
  failwith "Not yet implemented: get_line_join"

let set_miter_limit context l =
  failwith "Not yet implemented: set_miter_limit"

let get_miter_limit context =
  failwith "Not yet implemented: get_miter_limit"

let rel_move_to context ~x ~y =
  failwith "Not yet implemented: rel_move_to"

let rel_line_to context ~x ~y =
  failwith "Not yet implemented: rel_line_to"

let curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  failwith "Not yet implemented: curve_to"

let rel_curve_to context ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  failwith "Not yet implemented: rel_curve_to"

let rectangle context ~x ~y ~w ~h =
  failwith "Not yet implemented: rectangle"

let select_font_face context ?(slant=Upright) ?(weight=Normal) family =
  failwith "Not yet implemented: select_font_face"

let set_font_size context size =
  failwith "Not yet implemented: set_font_size"

let show_text context s =
  failwith "Not yet implemented: show_text"

let font_extents context =
  failwith "Not yet implemented: font_extents"

let text_extents context s =
  failwith "Not yet implemented: text_extents"

let paint ?(alpha=1.) context =
  failwith "Not yet implemented: paint"

let set_operator context operator =
  failwith "Not yet implemented: set_operator"

let get_operator context =
  failwith "Not yet implemented: get_operator"
