(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open StdLabels

(* @todo Use a real (unit) test framework *)

let () = Tests.MockTests.run ()

let () = begin
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
  |> List.iter ~f:(fun (cairo, mock) ->
    assert (Cairo.status_to_string cairo = CairoMock.status_to_string mock)
  )
end

let () =
  let module Tests = Tests.DrawingTests.Make(CairoMock) in
  Tests.tests
  |> List.iter ~f:(fun {Tests.draw; _} -> draw (CairoMock.create ()))

let () =
  let module Tests = Tests.DrawingTests.Make(Cairo) in
  Tests.tests
  |> List.iter ~f:(fun {Tests.name; width; height; draw; known_failure=_} ->
    let img = Cairo.Image.create Cairo.Image.ARGB32 ~width ~height in
    let ctx = Cairo.create img in
    draw ctx;
    Cairo.PNG.write img (Printf.sprintf "drawing_tests/in_command_line/%s.png" name);
  )
