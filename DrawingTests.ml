(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

let sprintf = OCamlStandard.Printf.sprintf

module Make(C: Context.S) = struct
  type test = {name: string; width: int; height: int; draw: C.context -> unit}

  let check_transform transform ctx (x, y) (x', y') =
    let (x'', y'') = transform ctx ~x ~y in
    if Fl.abs ((x'' -. x') /. x') > 0.01 || Fl.abs ((y'' -. y') /. y') > 0.01 then
      let transform =
        if phys_eq transform C.device_to_user_distance then "device_to_user_distance"
        else if phys_eq transform C.device_to_user then "device_to_user"
        else if phys_eq transform C.user_to_device_distance then "user_to_device_distance"
        else if phys_eq transform C.user_to_device then "user_to_device"
        else "unknown"
      in
      failwith (sprintf "Expected %s (%.2f, %.2f) = (%.2f, %.2f), got (%.2f, %.2f)" transform x y x' y' x'' y'')

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

  let make_save_restore name width height modify draw =
    let draw ctx =
      C.save ctx;
      modify ctx;
      C.restore ctx;
      draw ctx
    in
    make (sprintf "save restore: %s" name) width height draw

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
    make_current_point "arc" 100 80 (fun ctx ->
      C.arc ctx ~x:50. ~y:40. ~r:30. ~a1:0. ~a2:5.;
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
    make_current_point "move_to scale" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:20.;
      C.scale ctx ~x:4. ~y:2.;
      C.scale ctx ~x:0.3 ~y:0.7;
    );
    make_current_point "move_to scale identity_matrix" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:20.;
      C.scale ctx ~x:4. ~y:2.;
      C.identity_matrix ctx;
    );
    make_current_point "move_to translate" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:20.;
      C.translate ctx ~x:20. ~y:10.;
    );
    make_current_point "move_to translate identity_matrix" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:20.;
      C.translate ctx ~x:20. ~y:10.;
      C.identity_matrix ctx;
    );
    make_current_point "move_to rotate" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:20.;
      C.rotate ctx ~angle:1.;
    );
    make_current_point "move_to rotate identity_matrix" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:20.;
      C.rotate ctx ~angle:1.;
      C.identity_matrix ctx;
    );
    make_current_point "arc scale close" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:20. ~a1:1. ~a2:5.;
      C.scale ctx ~x:4. ~y:2.;
      C.Path.close ctx;
      C.stroke_preserve ctx;
    );
    make_current_point "arc scale identity_matrix close" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:20. ~a1:1. ~a2:5.;
      C.scale ctx ~x:4. ~y:2.;
      C.identity_matrix ctx;
      C.Path.close ctx;
      C.stroke_preserve ctx;
    );
    make "scale" 100 100 (fun ctx ->
      check_transform C.user_to_device ctx (10., 20.) (10., 20.);
      check_transform C.user_to_device_distance ctx (10., 20.) (10., 20.);
      check_transform C.device_to_user ctx (10., 20.) (10., 20.);
      check_transform C.device_to_user_distance ctx (10., 20.) (10., 20.);
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:30.;
      C.scale ctx ~x:3. ~y:0.5;
      check_transform C.user_to_device ctx (10., 20.) (30., 10.);
      check_transform C.user_to_device_distance ctx (10., 20.) (30., 10.);
      check_transform C.device_to_user ctx (9., 20.) (3., 40.);
      check_transform C.device_to_user_distance ctx (9., 20.) (3., 40.);
      C.line_to ctx ~x:30. ~y:180.;
      C.stroke ctx;
    );
    make "scale identity_matrix" 100 100 (fun ctx ->
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:30.;
      C.scale ctx ~x:3. ~y:0.5;
      C.line_to ctx ~x:30. ~y:180.;
      C.identity_matrix ctx;
      check_transform C.user_to_device ctx (10., 20.) (10., 20.);
      check_transform C.user_to_device_distance ctx (10., 20.) (10., 20.);
      check_transform C.device_to_user ctx (10., 20.) (10., 20.);
      check_transform C.device_to_user_distance ctx (10., 20.) (10., 20.);
      C.stroke ctx;
    );
    make "translate" 100 100 (fun ctx ->
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:30.;
      C.translate ctx ~x:20. ~y:(-30.);
      check_transform C.user_to_device ctx (10., 20.) (30., -10.);
      check_transform C.user_to_device_distance ctx (10., 20.) (10., 20.);
      check_transform C.device_to_user ctx (10., 20.) (-10., 50.);
      check_transform C.device_to_user_distance ctx (10., 20.) (10., 20.);
      C.line_to ctx ~x:70. ~y:120.;
      C.stroke ctx;
    );
    make "translate scale" 100 100 (fun ctx ->
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:30.;
      C.translate ctx ~x:20. ~y:(-30.);
      C.scale ctx ~x:3. ~y:0.5;
      check_transform C.user_to_device ctx (10., 20.) (50., -20.);
      check_transform C.user_to_device_distance ctx (10., 20.) (30., 10.);
      check_transform C.device_to_user ctx (38., 10.) (6., 80.);
      check_transform C.device_to_user_distance ctx (9., 20.) (3., 40.);
      C.line_to ctx ~x:20. ~y:200.;
      C.stroke ctx;
    );
    make "scale translate" 100 100 (fun ctx ->
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:30.;
      C.scale ctx ~x:3. ~y:0.5;
      C.translate ctx ~x:20. ~y:(-30.);
      check_transform C.user_to_device ctx (10., 20.) (90., -5.);
      check_transform C.user_to_device_distance ctx (10., 20.) (30., 10.);
      check_transform C.device_to_user ctx (9., 20.) (-17., 70.);
      check_transform C.device_to_user_distance ctx (9., 20.) (3., 40.);
      C.line_to ctx ~x:10. ~y:180.;
      C.stroke ctx;
    );
    make "rotate" 100 100 (fun ctx ->
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:30.;
      C.rotate ctx ~angle:0.1;
      check_transform C.user_to_device ctx (10., 20.) (7.95, 20.90);
      check_transform C.user_to_device_distance ctx (10., 20.) (7.95, 20.90);
      check_transform C.device_to_user ctx (9., 20.) (10.95, 19.00);
      check_transform C.device_to_user_distance ctx (9., 20.) (10.95, 19.00);
      C.line_to ctx ~x:80. ~y:70.;
      C.stroke ctx;
    );
    make "rotate translate scale" 100 100 (fun ctx ->
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:30.;
      C.rotate ctx ~angle:0.1;
      C.translate ctx ~x:10. ~y:20.;
      C.scale ctx ~x:(-2.) ~y:0.3;
      C.rotate ctx ~angle:0.1;
      check_transform C.user_to_device ctx (10., 20.) (-8.50, 25.55);
      check_transform C.user_to_device_distance ctx (10., 20.) (-16.45, 4.65);
      check_transform C.device_to_user ctx (9., 20.) (-0.81, -3.26);
      check_transform C.device_to_user_distance ctx (9., 20.) (0.87, 63.57);
      C.line_to ctx ~x:(-24.) ~y:205.;
      C.stroke ctx;
    );
    make_save_restore "line_width" 100 40 (fun ctx ->
      C.set_line_width ctx 10.;
    ) (fun ctx ->
      C.move_to ctx ~x:10. ~y:20.;
      C.line_to ctx ~x:90. ~y:20.;
      C.stroke ctx;
    );
    make_save_restore "source" 100 40 (fun ctx ->
      C.set_source_rgb ctx ~r:1. ~g:0. ~b:0.;
    ) (fun ctx ->
      C.move_to ctx ~x:10. ~y:20.;
      C.line_to ctx ~x:90. ~y:20.;
      C.stroke ctx;
    );
    make_save_restore "translate" 100 40 (fun ctx ->
      C.translate ctx ~x:50. ~y:20.;
    ) (fun ctx ->
      C.move_to ctx ~x:10. ~y:20.;
      C.line_to ctx ~x:90. ~y:20.;
      C.stroke ctx;
      check_transform C.user_to_device ctx (10., 10.) (10., 10.);
    );
    make_save_restore "scale" 100 40 (fun ctx ->
      C.scale ctx ~x:5. ~y:2.;
    ) (fun ctx ->
      C.move_to ctx ~x:10. ~y:20.;
      C.line_to ctx ~x:90. ~y:20.;
      C.stroke ctx;
      check_transform C.user_to_device ctx (10., 10.) (10., 10.);
    );
    make_save_restore "rotate" 100 40 (fun ctx ->
      C.rotate ctx ~angle:0.1;
    ) (fun ctx ->
      C.move_to ctx ~x:10. ~y:20.;
      C.line_to ctx ~x:90. ~y:20.;
      C.stroke ctx;
      check_transform C.user_to_device ctx (10., 10.) (10., 10.);
    );
    make_save_restore "move_to" 100 40 (fun ctx ->
      C.move_to ctx ~x:10. ~y:20.;
    ) (fun ctx ->
      C.line_to ctx ~x:90. ~y:20.;
      C.stroke ctx;
    );
    make "save restore: current point" 100 40 (fun ctx ->
      C.move_to ctx ~x:50. ~y:20.;
      C.save ctx;
      C.translate ctx ~x:20. ~y:10.;
      C.restore ctx;
      let (x, y) = C.Path.get_current_point ctx in
      C.arc ctx ~x ~y ~r:10. ~a1:0. ~a2:6.28;
      C.fill ctx
    );
    make "save restore: current point 2" 100 40 (fun ctx ->
      C.save ctx;
      C.translate ctx ~x:20. ~y:10.;
      C.move_to ctx ~x:30. ~y:10.;
      C.restore ctx;
      let (x, y) = C.Path.get_current_point ctx in
      C.arc ctx ~x ~y ~r:10. ~a1:0. ~a2:6.28;
      C.fill ctx
    );
    make "save restore: current point 3" 100 40 (fun ctx ->
      C.translate ctx ~x:5. ~y:15.;
      C.save ctx;
      C.translate ctx ~x:20. ~y:10.;
      C.move_to ctx ~x:25. ~y:(-5.);
      C.restore ctx;
      let (x, y) = C.Path.get_current_point ctx in
      C.arc ctx ~x ~y ~r:10. ~a1:0. ~a2:6.28;
      C.fill ctx
    );
    make "save restore: nested" 400 200 (fun ctx ->
      let rec aux = function
        | 0 ->
          C.line_to ctx ~x:1. ~y:0.;
          C.translate ctx ~x:1. ~y:0.;
        | n ->
          C.save ctx;
          C.scale ctx ~x:(1. /. 3.) ~y:(1. /. 3.);
          aux (n - 1);
          C.rotate ctx ~angle:(-.Math.pi /. 3.);
          aux (n - 1);
          C.rotate ctx ~angle:(2. *. Math.pi /. 3.);
          aux (n - 1);
          C.rotate ctx ~angle:(-.Math.pi /. 3.);
          aux (n - 1);
          C.restore ctx;
          C.translate ctx ~x:1. ~y:0.;
      in
      C.translate ctx ~x:0. ~y:150.;
      C.scale ctx ~x:400. ~y:400.;
      C.move_to ctx ~x:0. ~y:0.;
      aux 3;
      C.identity_matrix ctx;
      C.line_to ctx ~x:200. ~y:200.;
      C.Path.close ctx;
      C.stroke ctx;
    );
  ]
end
