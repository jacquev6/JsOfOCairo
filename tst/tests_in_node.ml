(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

let (canvas: (int -> int -> Dom_html.canvasElement Js.t) Js.constr) =
  Js.Unsafe.global##._Canvas

let (image: (Dom_html.imageElement Js.t) Js.constr) =
  Js.Unsafe.global##._Image

let (pixelmatch:
  Dom_html.canvasPixelArray Js.t
  -> Dom_html.canvasPixelArray Js.t
  -> Dom_html.canvasPixelArray Js.t
  -> int
  -> int
  -> <includeAA: bool Js_of_ocaml.Js.readonly_prop; threshold: float Js_of_ocaml.Js.readonly_prop> Js.t
  -> int
) =
  Js.Unsafe.global##.pixelmatch

let (writeTo: Dom_html.canvasElement Js.t -> Js.js_string Js.t -> unit) =
  Js.Unsafe.global##.writeTo

let test = "Tests in node.js" >:: [
  Tests.Unit.test;
  (
    let module T = Tests.Universal.Make(JsOfOCairo)(struct
      let name = "JsOfOCairo"

      let degraded = true

      let create () =
        JsOfOCairo.create (new%js canvas 10 10)
    end) in
    T.test
  );
  "Drawing tests on JsOfOCairo" >:: (
    let known_failures = [
      "paint with alpha 3";
      "paint with alpha 4";
      "text extents: move_to";
      "text: scale";
      "text extents: scale";
      "text: transform";
      "text extents: transform";
      "text extents: set_font_size";
      "text extents: set_font_face serif upright normal";
      "text extents: set_font_face serif upright bold";
      "text extents: set_font_face serif italic normal";
      "text extents: set_font_face serif oblique normal";
      "text extents: set_font_face sans-serif";
      "text extents: set_font_face cursive";
      "text extents: set_font_face fantasy";
      "text extents: set_font_face monospace";
    ] in
    let module T = Tests.Drawing.Make(JsOfOCairo) in
    T.tests
    |> Li.map ~f:(fun {T.name; width; height; draw; known_failure=_} ->
      let known_failure = Li.Poly.contains known_failures name in
      name >: (lazy (
        let cairo_image = new%js image in
        cairo_image##.src := (Js.string (Frmt.apply "Tests/Drawing/Cairo/%s.png" name));
        let cairo_canvas = new%js canvas width height in
        let cairo_context = cairo_canvas##getContext Dom_html._2d_ in
        cairo_context##drawImage cairo_image 0. 0.;
        let cairo_data = cairo_context##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

        let jsooc_canvas = new%js canvas width height in
        draw (JsOfOCairo.create jsooc_canvas);
        let jsooc_data = (jsooc_canvas##getContext Dom_html._2d_)##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

        let diff_canvas = new%js canvas width height in
        let diff_context = diff_canvas##getContext Dom_html._2d_ in
        let diff_data = diff_context##createImageData width height in

        let differences =
          pixelmatch
            cairo_data##.data
            jsooc_data##.data
            diff_data##.data
            width
            height
            (object%js (_) val threshold=0.09 val includeAA=false end)
        in
        diff_context##putImageData diff_data 0. 0.;

        let failure =
          if known_failure && differences = 0 then
            Some "Expected failure but drawings are identical"
          else if not known_failure && differences <> 0 then
            Some "Drawings are different"
          else
            None
        in
        failure
        |> Opt.iter ~f:(fun reason ->
          writeTo cairo_canvas (Js.string (Frmt.apply "Tests/Drawing/JsOfOCairo/%s.Cairo.png" name));
          writeTo jsooc_canvas (Js.string (Frmt.apply "Tests/Drawing/JsOfOCairo/%s.JsOfOCairo.png" name));
          writeTo diff_canvas (Js.string (Frmt.apply "Tests/Drawing/JsOfOCairo/%s.diff.png" name));
          fail "%s" reason
        )
      ))
    )
  );
]

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Js.Unsafe.global##.process##.exitCode :=
    match command_line_main ~argv test with
      | Exit.Success -> 0
      | Exit.Failure n -> n
