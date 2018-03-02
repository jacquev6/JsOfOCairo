(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

let test =
  "JsOfOCairo unit tests" >:: [
    Tests.MockTests.test;
    "status_to_string" >:: (
      [
        (Cairo.INVALID_RESTORE, CairoMock.INVALID_RESTORE);
        (Cairo.INVALID_POP_GROUP, CairoMock.INVALID_POP_GROUP);
        (Cairo.NO_CURRENT_POINT, CairoMock.NO_CURRENT_POINT);
        (Cairo.INVALID_MATRIX, CairoMock.INVALID_MATRIX);
        (Cairo.INVALID_STATUS, CairoMock.INVALID_STATUS);
        (Cairo.NULL_POINTER, CairoMock.NULL_POINTER);
        (Cairo.INVALID_STRING, CairoMock.INVALID_STRING);
        (Cairo.INVALID_PATH_DATA, CairoMock.INVALID_PATH_DATA);
        (Cairo.READ_ERROR, CairoMock.READ_ERROR);
        (Cairo.WRITE_ERROR, CairoMock.WRITE_ERROR);
        (Cairo.SURFACE_FINISHED, CairoMock.SURFACE_FINISHED);
        (Cairo.SURFACE_TYPE_MISMATCH, CairoMock.SURFACE_TYPE_MISMATCH);
        (Cairo.PATTERN_TYPE_MISMATCH, CairoMock.PATTERN_TYPE_MISMATCH);
        (Cairo.INVALID_CONTENT, CairoMock.INVALID_CONTENT);
        (Cairo.INVALID_FORMAT, CairoMock.INVALID_FORMAT);
        (Cairo.INVALID_VISUAL, CairoMock.INVALID_VISUAL);
        (Cairo.FILE_NOT_FOUND, CairoMock.FILE_NOT_FOUND);
        (Cairo.INVALID_DASH, CairoMock.INVALID_DASH);
        (Cairo.INVALID_DSC_COMMENT, CairoMock.INVALID_DSC_COMMENT);
        (Cairo.INVALID_INDEX, CairoMock.INVALID_INDEX);
        (Cairo.CLIP_NOT_REPRESENTABLE, CairoMock.CLIP_NOT_REPRESENTABLE);
        (Cairo.TEMP_FILE_ERROR, CairoMock.TEMP_FILE_ERROR);
        (Cairo.INVALID_STRIDE, CairoMock.INVALID_STRIDE);
        (Cairo.FONT_TYPE_MISMATCH, CairoMock.FONT_TYPE_MISMATCH);
        (Cairo.USER_FONT_IMMUTABLE, CairoMock.USER_FONT_IMMUTABLE);
        (Cairo.USER_FONT_ERROR, CairoMock.USER_FONT_ERROR);
        (Cairo.NEGATIVE_COUNT, CairoMock.NEGATIVE_COUNT);
        (Cairo.INVALID_CLUSTERS, CairoMock.INVALID_CLUSTERS);
        (Cairo.INVALID_SLANT, CairoMock.INVALID_SLANT);
        (Cairo.INVALID_WEIGHT, CairoMock.INVALID_WEIGHT);
        (Cairo.INVALID_SIZE, CairoMock.INVALID_SIZE);
        (Cairo.USER_FONT_NOT_IMPLEMENTED, CairoMock.USER_FONT_NOT_IMPLEMENTED);
        (Cairo.DEVICE_TYPE_MISMATCH, CairoMock.DEVICE_TYPE_MISMATCH);
        (Cairo.DEVICE_ERROR, CairoMock.DEVICE_ERROR);
        (Cairo.INVALID_MESH_CONSTRUCTION, CairoMock.INVALID_MESH_CONSTRUCTION);
        (Cairo.DEVICE_FINISHED, CairoMock.DEVICE_FINISHED);
        (Cairo.JBIG2_GLOBAL_MISSING, CairoMock.JBIG2_GLOBAL_MISSING);
      ]
      |> Li.map ~f:(fun (cairo, mock) ->
        let expected = Cairo.status_to_string cairo in
        expected >: (lazy (
          check_string ~expected (CairoMock.status_to_string mock)
        ))
      )
    );
    "Drawing tests on CairoMock" >:: (
      let module Tests = Tests.DrawingTests.Make(CairoMock) in
      Tests.tests
      |> Li.map ~f:(fun {Tests.name; draw; _} ->
        name >: (lazy (draw (CairoMock.create ())))
      )
    );
  ]

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv test)
