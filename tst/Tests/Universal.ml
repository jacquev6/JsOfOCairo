open General.Abbr
open Tst

module Make(C: CairoMock.S)(N: sig
  val name: string
  val degraded: bool
  val create: unit -> C.context
end) = struct
  open C

  let check_matrix =
    let equal {xx; xy; yx; yy; x0; y0} m =
      (
        Fl.approx_equal m.xx xx
        && Fl.approx_equal m.xy xy
        && Fl.approx_equal m.yx yx
        && Fl.approx_equal m.yy yy
        && Fl.approx_equal m.x0 x0
        && Fl.approx_equal m.y0 y0
      )
    and repr {xx; xy; yx; yy; x0; y0} =
      Frmt.apply "{xx=%f; xy=%f; yx=%f; yy=%f; x0=%f; y0=%f}" xx xy yx yy x0 y0
    in
    check ~equal ~repr

  let check_coords =
    let equal (x0, y0) (x1, y1) =
      (Fl.approx_equal x0 x1 && Fl.approx_equal y0 y1)
    and repr (x, y) =
      Frmt.apply "(%f, %f)" x y
    in
    check ~equal ~repr

  let test = ~:: "Universal tests on %s" N.name [
    "saved-and-restored settings" >:: (
      let make name setter getter check initial_value other_value other_values =
        name >: (lazy (
          let ctx = N.create () in
          check ~expected:initial_value (getter ctx);
          (initial_value::other_value::other_values)
          |> Li.iter ~f:(fun value ->
            setter ctx value;
            check ~expected:value (getter ctx);
            save ctx;
            check ~expected:value (getter ctx);
            setter ctx other_value;
            check ~expected:other_value (getter ctx);
            restore ctx;
            check ~expected:value (getter ctx);
          )
        ))
      in
      [
        make "line_width" set_line_width get_line_width check_float_exact 2. 1. [4.];
        make "miter_limit" set_miter_limit get_miter_limit check_float_exact 10. 5. [20.];
        (let repr = function
          (*BISECT-IGNORE-BEGIN*)
          | WINDING -> "WINDING"
          | EVEN_ODD -> "EVEN_ODD"
          (*BISECT-IGNORE-END*)
        in
        make "fill_rule" set_fill_rule get_fill_rule (check_poly ~repr) WINDING EVEN_ODD []);
        (let repr = function
          (*BISECT-IGNORE-BEGIN*)
          | BUTT -> "BUTT"
          | ROUND -> "ROUND"
          | SQUARE -> "SQUARE"
          (*BISECT-IGNORE-END*)
        in
        make "line_cap" set_line_cap get_line_cap (check_poly ~repr) BUTT ROUND [SQUARE]);
        (let repr = function
          (*BISECT-IGNORE-BEGIN*)
          | JOIN_MITER -> "JOIN_MITER"
          | JOIN_ROUND -> "JOIN_ROUND"
          | JOIN_BEVEL -> "JOIN_BEVEL"
          (*BISECT-IGNORE-END*)
        in
        make "line_join" set_line_join get_line_join (check_poly ~repr) JOIN_MITER JOIN_ROUND [JOIN_BEVEL]);
        (let repr = function
          (*BISECT-IGNORE-BEGIN*)
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
          (*BISECT-IGNORE-END*)
        in
        make "operator" set_operator get_operator (check_poly ~repr) OVER IN ([OUT; ATOP; DEST_OVER; DEST_IN; DEST_OUT; DEST_ATOP; XOR; ADD] @ if N.degraded then [] else [CLEAR; SOURCE; DEST; SATURATE]));
        "dash" >:: [
          (let repr dashes =
            (*BISECT-IGNORE-BEGIN*)
            dashes
            |> Li.of_array
            |> Li.map ~f:Fl.repr
            |> StrLi.join ~sep:"; "
            |> Frmt.apply "[|%s|]"
            (*BISECT-IGNORE-END*)
          in
          make "dashes" (fun c dashes -> set_dash c dashes) (fun c -> get_dash c |> Tu2.get_0) (check_poly ~repr) [||] [|1.; 2.|] ([[|3.; 4.; 5.; 6.|]; [|7.; 8.; 9.; 10.; 11.; 12.|]] @ if N.degraded then [] else [[|3.|]; [|4.; 5.; 6.|]]));
          make "offset" (fun c ofs -> set_dash c ~ofs [|10.; 10.|]) (fun c -> get_dash c |> Tu2.get_1) check_float_exact 0. 2. [3.];
        ];
      ]
    );
    "status_to_string" >:: (
      let make name status expected =
        name >: (lazy (
          check_string ~expected (status_to_string status)
        ))
      in
      [
        make "INVALID_RESTORE" INVALID_RESTORE "cairo_restore() without matching cairo_save()";
        make "INVALID_POP_GROUP" INVALID_POP_GROUP "no saved group to pop, i.e. cairo_pop_group() without matching cairo_push_group()";
        make "NO_CURRENT_POINT" NO_CURRENT_POINT "no current point defined";
        make "INVALID_MATRIX" INVALID_MATRIX "invalid matrix (not invertible)";
        make "INVALID_STATUS" INVALID_STATUS "invalid value for an input cairo_status_t";
        make "NULL_POINTER" NULL_POINTER "NULL pointer";
        make "INVALID_STRING" INVALID_STRING "input string not valid UTF-8";
        make "INVALID_PATH_DATA" INVALID_PATH_DATA "input path data not valid";
        make "READ_ERROR" READ_ERROR "error while reading from input stream";
        make "WRITE_ERROR" WRITE_ERROR "error while writing to output stream";
        make "SURFACE_FINISHED" SURFACE_FINISHED "the target surface has been finished";
        make "SURFACE_TYPE_MISMATCH" SURFACE_TYPE_MISMATCH "the surface type is not appropriate for the operation";
        make "PATTERN_TYPE_MISMATCH" PATTERN_TYPE_MISMATCH "the pattern type is not appropriate for the operation";
        make "INVALID_CONTENT" INVALID_CONTENT "invalid value for an input cairo_content_t";
        make "INVALID_FORMAT" INVALID_FORMAT "invalid value for an input cairo_format_t";
        make "INVALID_VISUAL" INVALID_VISUAL "invalid value for an input Visual*";
        make "FILE_NOT_FOUND" FILE_NOT_FOUND "file not found";
        make "INVALID_DASH" INVALID_DASH "invalid value for a dash setting";
        make "INVALID_DSC_COMMENT" INVALID_DSC_COMMENT "invalid value for a DSC comment";
        make "INVALID_INDEX" INVALID_INDEX "invalid index passed to getter";
        make "CLIP_NOT_REPRESENTABLE" CLIP_NOT_REPRESENTABLE "clip region not representable in desired format";
        make "TEMP_FILE_ERROR" TEMP_FILE_ERROR "error creating or writing to a temporary file";
        make "INVALID_STRIDE" INVALID_STRIDE "invalid value for stride";
        make "FONT_TYPE_MISMATCH" FONT_TYPE_MISMATCH "the font type is not appropriate for the operation";
        make "USER_FONT_IMMUTABLE" USER_FONT_IMMUTABLE "the user-font is immutable";
        make "USER_FONT_ERROR" USER_FONT_ERROR "error occurred in a user-font callback function";
        make "NEGATIVE_COUNT" NEGATIVE_COUNT "negative number used where it is not allowed";
        make "INVALID_CLUSTERS" INVALID_CLUSTERS "input clusters do not represent the accompanying text and glyph arrays";
        make "INVALID_SLANT" INVALID_SLANT "invalid value for an input cairo_font_slant_t";
        make "INVALID_WEIGHT" INVALID_WEIGHT "invalid value for an input cairo_font_weight_t";
        make "INVALID_SIZE" INVALID_SIZE "invalid value (typically too big) for the size of the input (surface, pattern, etc.)";
        make "USER_FONT_NOT_IMPLEMENTED" USER_FONT_NOT_IMPLEMENTED "user-font method not implemented";
        make "DEVICE_TYPE_MISMATCH" DEVICE_TYPE_MISMATCH "the device type is not appropriate for the operation";
        make "DEVICE_ERROR" DEVICE_ERROR "an operation to the device caused an unspecified error";
        make "INVALID_MESH_CONSTRUCTION" INVALID_MESH_CONSTRUCTION "invalid operation during mesh pattern construction";
        make "DEVICE_FINISHED" DEVICE_FINISHED "the target device has been finished";
        make "JBIG2_GLOBAL_MISSING" JBIG2_GLOBAL_MISSING "CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID used but no CAIRO_MIME_TYPE_JBIG2_GLOBAL data provided";
      ]
    );
    "transformations" >:: (
      let identity = {xx=1.; xy=0.; yx=0.; yy=1.; x0=0.; y0=0.} in
      let make name t expected =
        name >: (lazy (
          let ctx = N.create () in
          check_matrix ~expected:identity (get_matrix ctx);
          t ctx;
          check_matrix ~expected (get_matrix ctx);
          save ctx;
          check_matrix ~expected (get_matrix ctx);
          identity_matrix ctx;
          check_matrix ~expected:identity (get_matrix ctx);
          restore ctx;
          check_matrix ~expected (get_matrix ctx);
        ))
      in
      [
        make "translate" (translate ~x:2. ~y:3.) {xx=1.; xy=0.; yx=0.; yy=1.; x0=2.; y0=3.};
        make "scale" (scale ~x:2. ~y:3.) {xx=2.; xy=0.; yx=0.; yy=3.; x0=0.; y0=0.};
        make "rotate" (rotate ~angle:(Fl.pi /. 4.)) (let s = Fl.sqrt(2.) /. 2. in {xx=s; xy=(-.s); yx=s; yy=s; x0=0.; y0=0.});
        make "set_matrix" (fun c -> set_matrix c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.};
        make "transform" (fun c -> scale c ~x:2. ~y:3.; transform c {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.}) {xx=2.; xy=4.; yx=9.; yy=12.; x0=10.; y0=18.};
      ]
    );
    "coordinates transformation" >: (lazy (
      let ctx = N.create () in
      set_matrix ctx {xx=1.; xy=2.; yx=3.; yy=4.; x0=5.; y0=6.};
      check_coords ~expected:(-2., 2.) (device_to_user ctx ~x:7. ~y:8.);
      check_coords ~expected:(-6., 6.5) (device_to_user_distance ctx ~x:7. ~y:8.);
      check_coords ~expected:(28., 59.) (user_to_device ctx ~x:7. ~y:8.);
      check_coords ~expected:(23., 53.) (user_to_device_distance ctx ~x:7. ~y:8.);
    ));
  ]
end
