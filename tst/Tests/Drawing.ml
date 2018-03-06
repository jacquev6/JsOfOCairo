(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module Make(C: CairoMock.S) = struct
  type t = {name: string; width: int; height: int; draw: C.context -> unit}

  let make_simple name width height draw =
    {name; width; height; draw}

  let make_list name width height values draw =
    make_simple name width (height * Li.size values) (fun ctx ->
      values
      |> Li.iter ~f:(fun value ->
        C.save ctx;
        C.rectangle ctx ~x:0. ~y:0. ~w:(Fl.of_int width) ~h:(Fl.of_int height);
        C.clip ctx;

        draw value ctx;

        C.restore ctx;

        C.translate ctx ~x:0. ~y:(Fl.of_int height);
      )
    )

  let tests = ([
    make_list "set_line_width" 50 20 [5.; 10.] (fun line_width ctx ->
      C.set_line_width ctx line_width;
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:40. ~y:10.;
      C.stroke ctx;
    );
    make_list "set_miter_limit" 120 50 [8.; 9.; 10.] (fun miter_limit ctx ->
      C.set_miter_limit ctx miter_limit;
      C.set_line_join ctx C.JOIN_MITER;
      C.set_line_width ctx 5.;
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:10.;
      C.line_to ctx ~x:10. ~y:20.;
      C.move_to ctx ~x:10. ~y:30.;
      C.line_to ctx ~x:50. ~y:30.;
      C.line_to ctx ~x:10. ~y:39.;
      C.stroke ctx;
    );
    make_list "set_fill_rule" 60 60 C.[EVEN_ODD; WINDING] (fun fill_rule ctx ->
      C.set_fill_rule ctx fill_rule;
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:40. ~y:10.;
      C.line_to ctx ~x:40. ~y:50.;
      C.line_to ctx ~x:30. ~y:50.;
      C.line_to ctx ~x:30. ~y:20.;
      C.line_to ctx ~x:20. ~y:20.;
      C.line_to ctx ~x:20. ~y:30.;
      C.line_to ctx ~x:50. ~y:30.;
      C.line_to ctx ~x:50. ~y:40.;
      C.line_to ctx ~x:10. ~y:40.;
      C.fill ctx;
    );
    make_list "set_line_cap" 50 20 C.[ROUND; SQUARE; BUTT] (fun line_cap ctx ->
      C.set_line_cap ctx line_cap;
      C.set_line_width ctx 10.;
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:40. ~y:10.;
      C.stroke ctx;
    );
    make_list "set_line_join" 50 50 C.[JOIN_ROUND; JOIN_BEVEL; JOIN_MITER] (fun line_join ctx ->
      C.set_line_join ctx line_join;
      C.set_line_width ctx 10.;
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:40. ~y:10.;
      C.line_to ctx ~x:40. ~y:40.;
      C.stroke ctx;
    );
    make_list "set_operator" 90 90 C.[OVER; IN; OUT; ATOP; DEST_OVER; DEST_IN; DEST_OUT; DEST_ATOP; XOR; ADD] (fun operator ctx ->
      C.set_source_rgba ctx ~r:0. ~g:0. ~b:0.8 ~a:0.8;
      C.rectangle ctx ~x:10. ~y:10. ~w:50. ~h:50.;
      C.fill ctx;

      C.set_operator ctx operator;
      C.set_source_rgba ctx ~r:0.8 ~g:0. ~b:0. ~a:0.8;
      C.arc ctx ~x:50. ~y:50. ~r:30. ~a1:0. ~a2:(Fl.pi *. 2.);
      C.fill ctx;
    );
    make_list "set_dash" 100 20 [(0., [|5.; 7.; 9.; 11.|]); (0., [|10.; 2.|]); (3., [|10.; 2.|])] (fun (ofs, dashes) ctx ->
      C.set_dash ctx ~ofs dashes;
      C.set_line_width ctx 10.;
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:90. ~y:10.;
      C.stroke ctx;
    );
    make_simple "scale rotate translate save restore" 400 200 (fun ctx ->
      let rec aux = function
        | 0 ->
          C.line_to ctx ~x:1. ~y:0.;
          C.translate ctx ~x:1. ~y:0.;
        | n ->
          C.save ctx;
          C.scale ctx ~x:(1. /. 3.) ~y:(1. /. 3.);
          aux (n - 1);
          C.rotate ctx ~angle:(-.Fl.pi /. 3.);
          aux (n - 1);
          C.rotate ctx ~angle:(2. *. Fl.pi /. 3.);
          aux (n - 1);
          C.rotate ctx ~angle:(-.Fl.pi /. 3.);
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
    make_simple "scale identity_matrix" 100 100 (fun ctx ->
      C.move_to ctx ~x:10. ~y:10.;
      C.line_to ctx ~x:50. ~y:30.;
      C.scale ctx ~x:3. ~y:0.5;
      C.line_to ctx ~x:30. ~y:180.;
      C.identity_matrix ctx;
      C.stroke ctx;
    );
    (* @todo set_xxx before, during and after path *)
    (* @todo Demo all drawing functions
      ([rel_](curve|line|move)_to, arc[_negative], rectangle, Path.close),
      and (stroke|fill|clip)[_preserve]
      including clip and paint
    *)
    make_list
      "set_source_rgb"
      50 50
      [
        (0., 0., 0.); (0.5, 0.5, 0.5); (1., 1., 1.);
        (0.5, 0., 0.); (0., 0.5, 0.); (0., 0., 0.5);
        (1., 0., 0.); (0., 1., 0.); (0., 0., 1.);
        (0.5, 0.5, 0.); (0., 0.5, 0.5); (0.5, 0., 0.5);
        (1., 1., 0.); (0., 1., 1.); (1., 0., 1.);
      ]
      (fun (r, g, b) ctx ->
        C.set_source_rgb ctx ~r ~g ~b;
        C.rectangle ctx ~x:5. ~y:5. ~w:40. ~h:40.;
        C.fill ctx;
      )
    ;
    make_list
      "set_source_rgba"
      50 50
      [
        (0., 0., 0., 0.7); (0.5, 0.5, 0.5, 0.7); (1., 1., 1., 0.7);
        (0.5, 0., 0., 0.7); (0., 0.5, 0., 0.7); (0., 0., 0.5, 0.7);
        (1., 0., 0., 0.7); (0., 1., 0., 0.7); (0., 0., 1., 0.7);
        (0.5, 0.5, 0., 0.7); (0., 0.5, 0.5, 0.7); (0.5, 0., 0.5, 0.7);
        (1., 1., 0., 0.7); (0., 1., 1., 0.7); (1., 0., 1., 0.7);
        (0., 0., 0., 0.3); (0.5, 0.5, 0.5, 0.3); (1., 1., 1., 0.3);
        (0.5, 0., 0., 0.3); (0., 0.5, 0., 0.3); (0., 0., 0.5, 0.3);
        (1., 0., 0., 0.3); (0., 1., 0., 0.3); (0., 0., 1., 0.3);
        (0.5, 0.5, 0., 0.3); (0., 0.5, 0.5, 0.3); (0.5, 0., 0.5, 0.3);
        (1., 1., 0., 0.3); (0., 1., 1., 0.3); (1., 0., 1., 0.3);
      ]
      (fun (r, g, b, a) ctx ->
        C.set_source_rgb ctx ~r:0.6 ~g:0.6 ~b:0.9;
        C.paint ctx;
        C.set_source_rgba ctx ~r ~g ~b ~a;
        C.rectangle ctx ~x:5. ~y:5. ~w:40. ~h:40.;
        C.fill ctx;
      )
    ;
    make_simple "set_source linear gradient" 100 40 (fun ctx ->
      C.set_source_rgb ctx ~r:1. ~g:0. ~b:0.;
      C.paint ctx;
      let p = C.Pattern.create_linear ~x0:10. ~y0:20. ~x1:90. ~y1:30. in
      C.Pattern.add_color_stop_rgb p ~ofs:0. 1. 0. 0.;
      C.Pattern.add_color_stop_rgba p ~ofs:1. 0. 0. 1. 0.2;
      C.Pattern.add_color_stop_rgb p ~ofs:0.4 0. 1. 0.;
      C.set_source ctx p;
      C.rectangle ctx ~x:5. ~y:5. ~w:90. ~h:30.;
      C.fill ctx;
    );
    make_simple "set_source radial gradient" 200 200 (fun ctx ->
      let (x0, y0, r0, x1, y1, r1) = (50., 40., 30., 110., 140., 50.) in
      let p = C.Pattern.create_radial ~x0 ~y0 ~r0 ~x1 ~y1 ~r1 in
      C.Pattern.add_color_stop_rgb p ~ofs:0. 1. 0. 0.;
      C.Pattern.add_color_stop_rgb p ~ofs:1. 0. 0. 1.;
      C.set_source ctx p;
      C.paint ctx;
      C.set_source_rgb ctx ~r:0. ~g:0. ~b:0.;
      C.arc ctx ~x:x0 ~y:y0 ~r:r0 ~a1:0. ~a2:6.28;
      C.stroke ctx;
      C.arc ctx ~x:x1 ~y:y1 ~r:r1 ~a1:0. ~a2:6.28;
      C.stroke ctx;
    );
    make_simple "paint with alpha 1" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:6.28;
      C.fill ctx;
      C.set_source_rgb ctx ~r:0. ~g:1. ~b:1.;
      C.paint ctx ~alpha:0.5;
    );
    make_simple "paint with alpha 2" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:6.28;
      C.fill ctx;
      C.set_source_rgba ctx ~r:0. ~g:1. ~b:1. ~a:0.5;
      C.paint ctx ~alpha:0.5;
    );
    make_simple "paint with alpha 3" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:6.28;
      C.fill ctx;
      let p = C.Pattern.create_linear ~x0:0. ~y0:0. ~x1:100. ~y1:100. in
      C.Pattern.add_color_stop_rgb p ~ofs:0. 0. 1. 0.;
      C.Pattern.add_color_stop_rgb p ~ofs:1. 0. 0. 1.;
      C.set_source ctx p;
      C.paint ctx ~alpha:0.5;
    );
    make_simple "paint with alpha 4" 100 100 (fun ctx ->
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:6.28;
      C.fill ctx;
      let p = C.Pattern.create_linear ~x0:0. ~y0:0. ~x1:100. ~y1:100. in
      C.Pattern.add_color_stop_rgba p ~ofs:0. 0. 1. 0. 0.5;
      C.Pattern.add_color_stop_rgba p ~ofs:1. 0. 0. 1. 0.5;
      C.set_source ctx p;
      C.paint ctx ~alpha:0.5;
    );
    make_simple "set_font_size show_text" 100 50 (fun ctx ->
      C.move_to ctx ~x:10. ~y:40.;
      C.show_text ctx "Hello";
      C.set_font_size ctx 20.;
      C.show_text ctx "Hello";
    );
    (* make_simple "arc on more than 2 pi" 100 100 (fun ctx ->
      (* This test shows what I believe is a bug in Firefox:
      it seems to ignore the portion after 2 pi.
      The current point is consistent with Cairo's behavior, but the next call to line_to
      does not start from the current point.
      @todo Test with Safari (WebKit) and/or Internet Explorer and/or Edge *)
      C.move_to ctx ~x:50. ~y:60.;
      C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:(Fl.pi /. 2.) ~a2:(3. *. Fl.pi);
      let (x, y) = C.Path.get_current_point ctx in
      Tst.check_float ~precision:1e-3 ~expected:10. x;
      Tst.check_float ~precision:1e-3 ~expected:50. y;
      C.line_to ctx ~x:40. ~y:50.;
      C.set_line_width ctx 3.;
      C.stroke_preserve ctx;
    ); *)
    (* @todo Test show_text, set_font_size and set_font_face *)
  ])
end
