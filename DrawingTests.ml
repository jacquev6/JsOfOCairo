(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

let sprintf = OCamlStandard.Printf.sprintf

module Make(C: Context.S) = struct
  type test = {name: string; width: int; height: int; draw: C.context -> unit}

  let make name width height draw =
    {name; width; height; draw}

  let make_current_point name width height draw =
    let draw ctx =
      draw ctx;
      let (x, y) = C.Path.get_current_point ctx in
      C.Path.clear ctx;
      C.arc ctx ~x ~y ~r:10. ~a1:0. ~a2:6.28;
      C.fill ctx
    in
    make (sprintf "current point: %s" name) width height draw

  let tests = [
    make "move-line_to stroke" 100 100 (fun ctx ->
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:90.;
      C.line_to ctx ~x:90. ~y:10.;
      C.move_to ctx ~x:20. ~y:40.;
      C.line_to ctx ~x:80. ~y:60.;
      C.stroke ctx;
    );
    make "set-get_line_width set_source_rgb" 100 40 (fun ctx ->
      (* set before, during and after path *)
      C.set_line_width ctx 4.;
      C.set_source_rgb ctx ~r:0.9 ~g:0.1 ~b:0.1;
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:90. ~y:10.;
      C.stroke ctx;

      C.set_source_rgb ctx ~r:1. ~g:1.0 ~b:1.;
      C.set_line_width ctx 12.;
      C.move_to ctx ~x:10. ~y:20.;
      C.line_to ctx ~x:50. ~y:20.;
      C.set_source_rgb ctx ~r:0.1 ~g:0.9 ~b:0.1;
      C.set_line_width ctx 1.;
      C.line_to ctx ~x:90. ~y:20.;
      C.stroke ctx;

      C.move_to ctx ~x:10. ~y:30.;
      C.line_to ctx ~x:90. ~y:30.;
      C.set_source_rgb ctx ~r:0.1 ~g:0.1 ~b:0.9;
      C.set_line_width ctx 6.;
      C.stroke ctx;
    );
    make "arc stroke_preserve fill" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:5.;
      C.set_line_width ctx 10.;
      C.set_source_rgb ctx ~r:0.1 ~g:0.1 ~b:0.9;
      C.stroke_preserve ctx;
      C.set_source_rgb ctx ~r:0.1 ~g:0.9 ~b:0.1;
      C.fill ctx;
    );
    make "arc_negative stroke fill_preserve" 100 100 (fun ctx ->
      C.arc_negative ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:4.;
      C.set_line_width ctx 10.;
      C.set_source_rgb ctx ~r:0.9 ~g:0.1 ~b:0.1;
      C.fill_preserve ctx;
      C.set_source_rgb ctx ~r:0.1 ~g:0.1 ~b:0.9;
      C.stroke ctx;
    );
    make_current_point "initial" 100 20 (fun _ -> ());
    make_current_point "move_to" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:20.;
    );
    make_current_point "move_to clear" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:20.;
      C.Path.clear ctx;
    );
    make_current_point "line_to" 100 40 (fun ctx ->
      C.line_to ctx ~x:50. ~y:20.;
    );
    make_current_point "line_to clear" 100 40 (fun ctx ->
      C.line_to ctx ~x:50. ~y:20.;
      C.Path.clear ctx
    );
    make_current_point "line_to stroke" 100 40 (fun ctx ->
      C.move_to ctx ~x:0. ~y:0.;
      C.line_to ctx ~x:50. ~y:20.;
      C.stroke ctx;
    );
    make_current_point "line_to stroke_preserve" 100 40 (fun ctx ->
      C.move_to ctx ~x:0. ~y:0.;
      C.line_to ctx ~x:50. ~y:20.;
      C.stroke_preserve ctx;
    );
    make_current_point "arc" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:5.;
    );
    make_current_point "arc_negative" 100 100 (fun ctx ->
      C.arc_negative ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:3.;
    );
    make_current_point "arc fill" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:5.;
      C.fill ctx;
    );
    make_current_point "arc fill_preserve" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:5.;
      C.fill_preserve ctx;
    );
    make_current_point "move_to line_to close" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:10.;
      C.line_to ctx ~x:90. ~y:20.;
      C.line_to ctx ~x:50. ~y:30.;
      C.Path.close ctx;
      C.stroke_preserve ctx;
    );
    make_current_point "arc close" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:5.;
      C.Path.close ctx;
      C.stroke_preserve ctx;
    );
    make_current_point "arc_negative close" 100 100 (fun ctx ->
      C.arc_negative ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:3.;
      C.Path.close ctx;
      C.stroke_preserve ctx;
    );
  ]
end
