(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

let (pixelmatch:
  Js_of_ocaml.Dom_html.canvasPixelArray Js_of_ocaml.Js.t
  -> Js_of_ocaml.Dom_html.canvasPixelArray Js_of_ocaml.Js.t
  -> Js_of_ocaml.Dom_html.canvasPixelArray Js_of_ocaml.Js.t
  -> int
  -> int
  -> <includeAA: bool Js_of_ocaml.Js.readonly_prop; threshold: float Js_of_ocaml.Js.readonly_prop> Js_of_ocaml.Js.t
  -> int
) =
  Js_of_ocaml.Js.Unsafe.global##.pixelmatch

module DrawingTests: sig
  val onload: (Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t StrSoMap.t -> unit) -> unit
end = struct
  let tests_count = Li.size Tests.drawing_tests

  let onload callback =
    let cairo_images = ref StrSoMap.empty in
    Tests.drawing_tests
    |> Li.iter ~f:(fun name ->
      let cairo_image = Js_of_ocaml.Dom_html.createImg Js_of_ocaml.Dom_html.document in
      cairo_image##.src := Js_of_ocaml.Js.string (Frmt.apply "Tests/Drawing/Cairo/%s.png" name);
      cairo_image##.onload := (Js_of_ocaml.Dom.handler (fun _ ->
        cairo_images := StrSoMap.replace !cairo_images ~k:name ~v:cairo_image;
        if StrSoMap.size !cairo_images = tests_count then callback !cairo_images;
        Js_of_ocaml.Js._true
      ));
    )
end

let drawing_test_ps = ref StrSoMap.empty

let result_to_element result =
  let rec aux is_in_drawing_tests = function
    | Result.Single {label; status} ->
      let li = Js_of_ocaml.Dom_html.createLi Js_of_ocaml.Dom_html.document in
      let p = Js_of_ocaml.Dom_html.createP Js_of_ocaml.Dom_html.document in
      li##.className := Js_of_ocaml.Js.string (if status = Result.Status.Success then "success" else "failure");
      p##.textContent := Js_of_ocaml.Js.some (Js_of_ocaml.Js.string (Frmt.apply "%s: %s" label (Result.Status.to_string status)));
      Js_of_ocaml.Dom.appendChild li p;
      if is_in_drawing_tests then Js_of_ocaml.Dom.appendChild li (StrSoMap.get !drawing_test_ps ~k:label);
      li
    | Result.Group {name; children; counts={Result.Counts.successes; failures; errors}} ->
      let li = Js_of_ocaml.Dom_html.createLi Js_of_ocaml.Dom_html.document in
      let p = Js_of_ocaml.Dom_html.createP Js_of_ocaml.Dom_html.document in
      li##.className := Js_of_ocaml.Js.string (if failures + errors = 0 then "success" else "failure");
      p##.textContent := Js_of_ocaml.Js.some (Js_of_ocaml.Js.string (Frmt.apply "%s: (successes: %i, failures: %i, errors: %i)" name successes failures errors));
      Js_of_ocaml.Dom.appendChild li p;
      let ul = Js_of_ocaml.Dom_html.createUl Js_of_ocaml.Dom_html.document in
      Js_of_ocaml.Dom.appendChild li ul;
      children
      |> Li.iter ~f:(fun child ->
        Js_of_ocaml.Dom.appendChild ul (aux (name = "Drawing tests on JsOfOCairo") child)
      );
      li
  in
  let ul = Js_of_ocaml.Dom_html.createUl Js_of_ocaml.Dom_html.document in
  Js_of_ocaml.Dom.appendChild ul (aux false result);
  ul

let () = DrawingTests.onload (fun cairo_images ->
  let module T = Tests.Make(struct
    let title = "Tests in browser"

    module C = JsOfOCairo

    module N = struct
      let name = "JsOfOCairo"

      let create () =
        let canvas = Js_of_ocaml.Dom_html.createCanvas Js_of_ocaml.Dom_html.document in
        canvas##.width := 10;
        canvas##.height := 10;
        JsOfOCairo.create canvas

      let backend = `Browser
    end

    module DrawingTest(T: sig
      type t = {name: string; width: int; height: int; draw: C.context -> unit}
    end) = struct
      let run {T.name; width; height; draw} =
        let cairo_image = StrSoMap.get cairo_images ~k:name in
        let cairo_canvas = Js_of_ocaml.Dom_html.createCanvas Js_of_ocaml.Dom_html.document in
        cairo_canvas##.width := width;
        cairo_canvas##.height := height;
        let cairo_context = cairo_canvas##getContext Js_of_ocaml.Dom_html._2d_ in
        cairo_context##drawImage cairo_image 0. 0.;
        let cairo_data = cairo_context##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

        let jsooc_canvas = Js_of_ocaml.Dom_html.createCanvas Js_of_ocaml.Dom_html.document in
        jsooc_canvas##.width := width;
        jsooc_canvas##.height := height;
        draw (JsOfOCairo.create jsooc_canvas);
        let jsooc_data = (jsooc_canvas##getContext Js_of_ocaml.Dom_html._2d_)##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

        let diff_canvas = Js_of_ocaml.Dom_html.createCanvas Js_of_ocaml.Dom_html.document in
        diff_canvas##.width := width;
        diff_canvas##.height := height;
        let diff_context = diff_canvas##getContext Js_of_ocaml.Dom_html._2d_ in
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

        let p = Js_of_ocaml.Dom_html.createP Js_of_ocaml.Dom_html.document in
        (* p##.textContent := Js_of_ocaml.Js.some (Js_of_ocaml.Js.string (Frmt.apply "Some info for %s" name)); *)
        Js_of_ocaml.Dom.appendChild p cairo_canvas;
        Js_of_ocaml.Dom.appendChild p jsooc_canvas;
        Js_of_ocaml.Dom.appendChild p diff_canvas;

        drawing_test_ps := StrSoMap.replace !drawing_test_ps ~k:name ~v:p;

        check_int ~expected:0 differences
    end

    module Limitation(L: sig
      type t = {name: string; width: int; height: int; draws: (C.context -> string list) list}
    end) = struct
      let make_col innerHTML =
        let div = Js_of_ocaml.Dom_html.(createDiv document) in
        div##setAttribute (Js_of_ocaml.Js.string "class") (Js_of_ocaml.Js.string "col");
        div##.innerHTML := Js_of_ocaml.Js.string innerHTML;
        div

      let run {L.name; width; height; draws} =
        let canvas = Js_of_ocaml.Dom_html.(createCanvas document) in
        canvas##.width := width;
        canvas##.height := height;
        let scripts ~i draw =
          let cairo = Frmt.apply "<div class=\"cairo_pre\" data-src=\"Tests/Limitations/%s.%i.txt\"></div>" name i
          and jsooc =
            draw (JsOfOCairo.create canvas)
            |> Li.map ~f:(Frmt.apply "%s\n")
            |> StrLi.join
            |> Frmt.apply "<pre>%s</pre>"
          in
          (cairo, jsooc)
        in
        let scripts =
          match draws with
            | [draw] ->
              [scripts ~i:0 draw]
            | _ ->
              draws
              |> Li.map_i ~f:(fun ~i draw ->
                let (cairo, jsooc) = scripts ~i draw
                and header = Frmt.apply "<p>Context n°%i:</p>" i in
                (header ^ cairo, header ^ jsooc)
              )
        in
        let cairo_col =
          let script =
            scripts
            |> Li.map ~f:Tu2.get_0
            |> StrLi.join
          in
          make_col (Frmt.apply "<h3>Cairo:</h3>%s<img src=\"Tests/Limitations/%s.png\"></img>" script name)
        and jsooc_col =
          let script =
            scripts
            |> Li.map ~f:Tu2.get_1
            |> StrLi.join
          in
          let col = make_col (Frmt.apply "<h3>JsOfOCairo:</h3>%s" script) in
          Js_of_ocaml.Dom.appendChild col canvas;
          col
        in
        let container = Js_of_ocaml.Dom_html.getElementById (Frmt.apply "limitations_%s" name) in
        Js_of_ocaml.Dom.appendChild container cairo_col;
        Js_of_ocaml.Dom.appendChild container jsooc_col
    end
  end) in
  T.test
  |> Test.run
  |> result_to_element
  |> Js_of_ocaml.Dom.appendChild (Js_of_ocaml.Dom_html.getElementById "tests_in_browser");
  ignore (Js_of_ocaml.Js.Unsafe.eval_string {|
    jQuery("#tests_in_browser ul ul ul").hide();
    jQuery("#tests_in_browser p").click(function() {
      jQuery(this).parent().children("ul").slideToggle();
    });
    jQuery("div.cairo_pre").each(function() {
      var div = jQuery(this);
      div.load(div.data("src"));
    })
  |});
)
