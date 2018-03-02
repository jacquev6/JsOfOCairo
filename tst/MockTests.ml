open General.Abbr
open Tst

open CairoMock

let make_n f expected =
  "some name" >: (lazy (
    let c = create () in
    ignore (f c);
    let actual = calls c in
    check_string_list ~expected actual
  ))

let make f expected =
  make_n f [expected]

let test = "CairoMock" >:: [
  make_n (fun c -> save c; restore c) ["save"; "restore"];

  make (fun c -> scale c ~x:3. ~y:2.) "scale ~x:3.00 ~y:2.00";
  make (fun c -> translate c ~x:3. ~y:2.) "translate ~x:3.00 ~y:2.00";
  make (fun c -> rotate c ~angle:3.) "rotate ~angle:3.00";
  make (fun c -> transform c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) "transform {xx=1.00; xy=2.00; yx=3.00; yy=4.00; x0=5.00; y0=6.00}";
  make (fun c -> set_matrix c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) "set_matrix {xx=1.00; xy=2.00; yx=3.00; yy=4.00; x0=5.00; y0=6.00}";
  make (fun c -> identity_matrix c) "identity_matrix";
  make (fun c -> get_matrix c) "get_matrix -> {xx=1.00; xy=0.00; yx=0.00; yy=1.00; x0=0.00; y0=0.00}";
  make (fun c -> user_to_device c ~x:2. ~y:3.) "user_to_device ~x:2.00 ~y:3.00 -> (2.00, 3.00)";
  make (fun c -> user_to_device_distance c ~x:2. ~y:3.) "user_to_device_distance ~x:2.00 ~y:3.00 -> (2.00, 3.00)";
  make (fun c -> device_to_user c ~x:2. ~y:3.) "device_to_user ~x:2.00 ~y:3.00 -> (2.00, 3.00)";
  make (fun c -> device_to_user_distance c ~x:2. ~y:3.) "device_to_user_distance ~x:2.00 ~y:3.00 -> (2.00, 3.00)";

  make (fun c -> move_to c ~x:4.05 ~y:2.957) "move_to ~x:4.05 ~y:2.96";
  make_n (fun c -> move_to c ~x:1. ~y:2.; rel_move_to c ~x:3. ~y:4.) ["move_to ~x:1.00 ~y:2.00"; "rel_move_to ~x:3.00 ~y:4.00"];
  make (fun c -> line_to c ~x:4.05 ~y:2.957) "line_to ~x:4.05 ~y:2.96";
  make_n (fun c -> move_to c ~x:1. ~y:2.; rel_line_to c ~x:3. ~y:4.) ["move_to ~x:1.00 ~y:2.00"; "rel_line_to ~x:3.00 ~y:4.00"];
  make (fun c -> curve_to c ~x1:1. ~y1:2. ~x2:3. ~y2:4. ~x3:5. ~y3:6.) "curve_to ~x1:1.00 ~y1:2.00 ~x2:3.00 ~y2:4.00 ~x3:5.00 ~y3:6.00";
  make_n (fun c -> move_to c ~x:1. ~y:2.; rel_curve_to c ~x1:1. ~y1:2. ~x2:3. ~y2:4. ~x3:5. ~y3:6.) ["move_to ~x:1.00 ~y:2.00"; "rel_curve_to ~x1:1.00 ~y1:2.00 ~x2:3.00 ~y2:4.00 ~x3:5.00 ~y3:6.00"];
  make (fun c -> rectangle c ~x:2. ~y:3. ~w:4. ~h:5.) "rectangle ~x:2.00 ~y:3.00 ~w:4.00 ~h:5.00";
  make (fun c -> arc c ~x:1. ~y:2. ~r:3. ~a1:4. ~a2:5.) "arc ~x:1.00 ~y:2.00 ~r:3.00 ~a1:4.00 ~a2:5.00";
  make (fun c -> arc_negative c ~x:1. ~y:2. ~r:3. ~a1:4. ~a2:5.) "arc_negative ~x:1.00 ~y:2.00 ~r:3.00 ~a1:4.00 ~a2:5.00";
  make (fun c -> Path.close c) "Path.close";
  make (fun c -> Path.clear c) "Path.clear";
  make (fun c -> Path.get_current_point c) "Path.get_current_point -> (0.00, 0.00)";

  make (fun c -> stroke c) "stroke";
  make (fun c -> stroke_preserve c) "stroke_preserve";
  make (fun c -> fill c) "fill";
  make (fun c -> fill_preserve c) "fill_preserve";
  make (fun c -> clip c) "clip";
  make (fun c -> clip_preserve c) "clip_preserve";
  make (fun c -> paint c) "paint ~alpha:1.00";
  make (fun c -> paint c ~alpha:0.5) "paint ~alpha:0.50";

  make (fun c -> set_line_width c 3.) "set_line_width 3.00";
  make (fun c -> get_line_width c) "get_line_width -> 2.00";
  make (fun c -> set_dash c [|2.; 3.|]) "set_dash ~ofs:0.00 [|2.00; 3.00|]";
  make (fun c -> get_dash c) "get_dash -> ([||], 0.00)";
  make (fun c -> set_fill_rule c EVEN_ODD) "set_fill_rule EVEN_ODD";
  make (fun c -> get_fill_rule c) "get_fill_rule -> WINDING";
  make (fun c -> set_line_cap c ROUND) "set_line_cap ROUND";
  make (fun c -> get_line_cap c) "get_line_cap -> BUTT";
  make (fun c -> set_line_join c JOIN_ROUND) "set_line_join JOIN_ROUND";
  make (fun c -> get_line_join c) "get_line_join -> JOIN_MITER";
  make (fun c -> set_miter_limit c 3.) "set_miter_limit 3.00";
  make (fun c -> get_miter_limit c) "get_miter_limit -> 10.00";
  make (fun c -> set_operator c DEST) "set_operator DEST";
  make (fun c -> set_operator c CLEAR) "set_operator CLEAR";
  make (fun c -> set_operator c SOURCE) "set_operator SOURCE";
  make (fun c -> set_operator c SATURATE) "set_operator SATURATE";
  make (fun c -> get_operator c) "get_operator -> OVER";

  make (fun c -> set_source_rgb c ~r:0.5 ~g:0.6 ~b:0.7) "set_source_rgb ~r:0.50 ~g:0.60 ~b:0.70";
  make (fun c -> set_source_rgba c ~r:0.5 ~g:0.6 ~b:0.7 ~a:0.8) "set_source_rgba ~r:0.50 ~g:0.60 ~b:0.70 ~a:0.80";
  make (fun c -> set_source c (Pattern.create_rgb ~r:0.5 ~g:0.6 ~b:0.7)) "set_source (Rgba {r=0.50; g=0.60; b=0.70; a=1.00})";
  make (fun c -> get_source c) "get_source -> (Rgba {r=0.00; g=0.00; b=0.00; a=1.00})";

  make (fun c -> set_font_size c 3.) "set_font_size 3.00";
  make (fun c -> select_font_face c "foo-bar") "select_font_face ~slant:Upright ~weight:Normal \"foo-bar\"";
  make (fun c -> select_font_face c ~slant:Italic ~weight:Bold "foo-bar") "select_font_face ~slant:Italic ~weight:Bold \"foo-bar\"";
  make (fun c -> show_text c "flibidiboo") "show_text \"flibidiboo\"";
  make (fun c -> text_extents c "abcd") "text_extents \"abcd\" -> {x_bearing=0.00; y_bearing=0.00; width=32.00; height=10.00; x_advance=32.00; y_advance=0.00}";
  make (fun c -> font_extents c) "font_extents -> {ascent=10.00; descent=2.50; baseline=0.00; max_x_advance=20.00; max_y_advance=0.00}";
]
