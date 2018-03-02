(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

module DrawingTests = DrawingTests.Make(CairoMock)

let test =
  "CairoMock" >:: [
    "drawing functions" >:: CairoMock.(
      let make_n name fs expected =
        name >: (lazy (
          let c = create () in
          Li.iter ~f:(fun f -> ignore (f c)) fs;
          let actual = calls c in
          check_string_list ~expected actual
        ))
      in
      let make name f expected =
        make_n name [f] [expected]
      in
      [
        make_n "save, restore" [save; restore] ["save"; "restore"];

        make "scale" (scale ~x:3. ~y:2.) "scale ~x:3.00 ~y:2.00";
        make "translate" (translate ~x:3. ~y:2.) "translate ~x:3.00 ~y:2.00";
        make "rotate" (rotate ~angle:3.) "rotate ~angle:3.00";
        make "transform" (fun c -> transform c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) "transform {xx=1.00; xy=2.00; yx=3.00; yy=4.00; x0=5.00; y0=6.00}";
        make "set_matrix" (fun c -> set_matrix c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) "set_matrix {xx=1.00; xy=2.00; yx=3.00; yy=4.00; x0=5.00; y0=6.00}";
        make "identity_matrix" identity_matrix "identity_matrix";
        make "get_matrix" get_matrix "get_matrix -> {xx=1.00; xy=0.00; yx=0.00; yy=1.00; x0=0.00; y0=0.00}";
        make "user_to_device" (user_to_device ~x:2. ~y:3.) "user_to_device ~x:2.00 ~y:3.00 -> (2.00, 3.00)";
        make "user_to_device_distance" (user_to_device_distance ~x:2. ~y:3.) "user_to_device_distance ~x:2.00 ~y:3.00 -> (2.00, 3.00)";
        make "device_to_user" (device_to_user ~x:2. ~y:3.) "device_to_user ~x:2.00 ~y:3.00 -> (2.00, 3.00)";
        make "device_to_user_distance" (device_to_user_distance ~x:2. ~y:3.) "device_to_user_distance ~x:2.00 ~y:3.00 -> (2.00, 3.00)";

        make "move_to" (move_to ~x:4.05 ~y:2.957) "move_to ~x:4.05 ~y:2.96";
        make_n "rel_move_to" [move_to ~x:1. ~y:2.; rel_move_to ~x:3. ~y:4.] ["move_to ~x:1.00 ~y:2.00"; "rel_move_to ~x:3.00 ~y:4.00"];
        make "line_to" (line_to ~x:4.05 ~y:2.957) "line_to ~x:4.05 ~y:2.96";
        make_n "rel_line_to" [move_to ~x:1. ~y:2.; rel_line_to ~x:3. ~y:4.] ["move_to ~x:1.00 ~y:2.00"; "rel_line_to ~x:3.00 ~y:4.00"];
        make "curve_to" (curve_to ~x1:1. ~y1:2. ~x2:3. ~y2:4. ~x3:5. ~y3:6.) "curve_to ~x1:1.00 ~y1:2.00 ~x2:3.00 ~y2:4.00 ~x3:5.00 ~y3:6.00";
        make_n "rel_curve_to" [move_to ~x:1. ~y:2.; rel_curve_to ~x1:1. ~y1:2. ~x2:3. ~y2:4. ~x3:5. ~y3:6.] ["move_to ~x:1.00 ~y:2.00"; "rel_curve_to ~x1:1.00 ~y1:2.00 ~x2:3.00 ~y2:4.00 ~x3:5.00 ~y3:6.00"];
        make "rectangle" (rectangle ~x:2. ~y:3. ~w:4. ~h:5.) "rectangle ~x:2.00 ~y:3.00 ~w:4.00 ~h:5.00";
        make "arc" (arc ~x:1. ~y:2. ~r:3. ~a1:4. ~a2:5.) "arc ~x:1.00 ~y:2.00 ~r:3.00 ~a1:4.00 ~a2:5.00";
        make "arc_negative" (arc_negative ~x:1. ~y:2. ~r:3. ~a1:4. ~a2:5.) "arc_negative ~x:1.00 ~y:2.00 ~r:3.00 ~a1:4.00 ~a2:5.00";
        make "Path.close" Path.close "Path.close";
        make "Path.clear" Path.clear "Path.clear";
        make "Path.get_current_point" Path.get_current_point "Path.get_current_point -> (0.00, 0.00)";

        make "stroke" stroke "stroke";
        make "stroke_preserve" stroke_preserve "stroke_preserve";
        make "fill" fill "fill";
        make "fill_preserve" fill_preserve "fill_preserve";
        make "clip" clip "clip";
        make "clip_preserve" clip_preserve "clip_preserve";
        make "paint" paint "paint ~alpha:1.00";
        make "paint with alpha" (paint ~alpha:0.5) "paint ~alpha:0.50";

        make "set_line_width" (fun c -> set_line_width c 3.) "set_line_width 3.00";
        make "get_line_width" get_line_width "get_line_width -> 2.00";
        make "set_dash" (fun c -> set_dash c [|2.; 3.|]) "set_dash ~ofs:0.00 [|2.00; 3.00|]";
        make "get_dash" get_dash "get_dash -> ([||], 0.00)";
        make "set_fill_rule" (fun c -> set_fill_rule c EVEN_ODD) "set_fill_rule EVEN_ODD";
        make "get_fill_rule" get_fill_rule "get_fill_rule -> WINDING";
        make "set_line_cap" (fun c -> set_line_cap c ROUND) "set_line_cap ROUND";
        make "get_line_cap" get_line_cap "get_line_cap -> BUTT";
        make "set_line_join" (fun c -> set_line_join c JOIN_ROUND) "set_line_join JOIN_ROUND";
        make "get_line_join" get_line_join "get_line_join -> JOIN_MITER";
        make "set_miter_limit" (fun c -> set_miter_limit c 3.) "set_miter_limit 3.00";
        make "get_miter_limit" get_miter_limit "get_miter_limit -> 10.00";
        make "set_operator" (fun c -> set_operator c DEST) "set_operator DEST";
        make "get_operator" get_operator "get_operator -> OVER";

        make "set_source_rgb" (set_source_rgb ~r:0.5 ~g:0.6 ~b:0.7) "set_source_rgb ~r:0.50 ~g:0.60 ~b:0.70";
        make "set_source_rgba" (set_source_rgba ~r:0.5 ~g:0.6 ~b:0.7 ~a:0.8) "set_source_rgba ~r:0.50 ~g:0.60 ~b:0.70 ~a:0.80";
        make "set_source" (fun c -> set_source c (Pattern.create_rgb ~r:0.5 ~g:0.6 ~b:0.7)) "set_source (Rgba {r=0.50; g=0.60; b=0.70; a=1.00})";
        make "get_source" get_source "get_source -> (Rgba {r=0.00; g=0.00; b=0.00; a=1.00})";

        make "set_font_size" (fun c -> set_font_size c 3.) "set_font_size 3.00";
        make "select_font_face" (fun c -> select_font_face c "foo-bar") "select_font_face ~slant:Upright ~weight:Normal \"foo-bar\"";
        make "select_font_face with slant and weight" (fun c -> select_font_face c ~slant:Italic ~weight:Bold "foo-bar") "select_font_face ~slant:Italic ~weight:Bold \"foo-bar\"";
        make "show_text" (fun c -> show_text c "flibidiboo") "show_text \"flibidiboo\"";
        make "text_extents" (fun c -> text_extents c "abcd") "text_extents \"abcd\" -> {x_bearing=0.00; y_bearing=0.00; width=32.00; height=10.00; x_advance=32.00; y_advance=0.00}";
        make "font_extents" font_extents "font_extents -> {ascent=10.00; descent=2.50; baseline=0.00; max_x_advance=20.00; max_y_advance=0.00}";
      ]
    );
    "status_to_string" >:: (
      let make cairo mock =
        let expected = Cairo.status_to_string cairo in
        expected >: (lazy (
          check_string ~expected (CairoMock.status_to_string mock)
        ))
      in
      [
        make Cairo.INVALID_RESTORE CairoMock.INVALID_RESTORE;
        make Cairo.INVALID_POP_GROUP CairoMock.INVALID_POP_GROUP;
        make Cairo.NO_CURRENT_POINT CairoMock.NO_CURRENT_POINT;
        make Cairo.INVALID_MATRIX CairoMock.INVALID_MATRIX;
        make Cairo.INVALID_STATUS CairoMock.INVALID_STATUS;
        make Cairo.NULL_POINTER CairoMock.NULL_POINTER;
        make Cairo.INVALID_STRING CairoMock.INVALID_STRING;
        make Cairo.INVALID_PATH_DATA CairoMock.INVALID_PATH_DATA;
        make Cairo.READ_ERROR CairoMock.READ_ERROR;
        make Cairo.WRITE_ERROR CairoMock.WRITE_ERROR;
        make Cairo.SURFACE_FINISHED CairoMock.SURFACE_FINISHED;
        make Cairo.SURFACE_TYPE_MISMATCH CairoMock.SURFACE_TYPE_MISMATCH;
        make Cairo.PATTERN_TYPE_MISMATCH CairoMock.PATTERN_TYPE_MISMATCH;
        make Cairo.INVALID_CONTENT CairoMock.INVALID_CONTENT;
        make Cairo.INVALID_FORMAT CairoMock.INVALID_FORMAT;
        make Cairo.INVALID_VISUAL CairoMock.INVALID_VISUAL;
        make Cairo.FILE_NOT_FOUND CairoMock.FILE_NOT_FOUND;
        make Cairo.INVALID_DASH CairoMock.INVALID_DASH;
        make Cairo.INVALID_DSC_COMMENT CairoMock.INVALID_DSC_COMMENT;
        make Cairo.INVALID_INDEX CairoMock.INVALID_INDEX;
        make Cairo.CLIP_NOT_REPRESENTABLE CairoMock.CLIP_NOT_REPRESENTABLE;
        make Cairo.TEMP_FILE_ERROR CairoMock.TEMP_FILE_ERROR;
        make Cairo.INVALID_STRIDE CairoMock.INVALID_STRIDE;
        make Cairo.FONT_TYPE_MISMATCH CairoMock.FONT_TYPE_MISMATCH;
        make Cairo.USER_FONT_IMMUTABLE CairoMock.USER_FONT_IMMUTABLE;
        make Cairo.USER_FONT_ERROR CairoMock.USER_FONT_ERROR;
        make Cairo.NEGATIVE_COUNT CairoMock.NEGATIVE_COUNT;
        make Cairo.INVALID_CLUSTERS CairoMock.INVALID_CLUSTERS;
        make Cairo.INVALID_SLANT CairoMock.INVALID_SLANT;
        make Cairo.INVALID_WEIGHT CairoMock.INVALID_WEIGHT;
        make Cairo.INVALID_SIZE CairoMock.INVALID_SIZE;
        make Cairo.USER_FONT_NOT_IMPLEMENTED CairoMock.USER_FONT_NOT_IMPLEMENTED;
        make Cairo.DEVICE_TYPE_MISMATCH CairoMock.DEVICE_TYPE_MISMATCH;
        make Cairo.DEVICE_ERROR CairoMock.DEVICE_ERROR;
        make Cairo.INVALID_MESH_CONSTRUCTION CairoMock.INVALID_MESH_CONSTRUCTION;
        make Cairo.DEVICE_FINISHED CairoMock.DEVICE_FINISHED;
        make Cairo.JBIG2_GLOBAL_MISSING CairoMock.JBIG2_GLOBAL_MISSING;
      ]
    );
    "Drawing tests" >:: (
      DrawingTests.tests
      |> Li.map ~f:(fun {DrawingTests.name; draw; _} ->
        name >: (lazy (draw (CairoMock.create ())))
      )
    );
  ]

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv test)
