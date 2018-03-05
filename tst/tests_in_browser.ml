(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

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


module DrawingTests: sig
  type t = {
    name: string;
    differences: int;
    p: Dom_html.paragraphElement Js.t;
  }

  val on_done: (t list -> unit) -> unit
end = struct
  module T = Tests.Drawing.Make(JsOfOCairo)

  let tests_count = Li.size T.tests

  type t = {
    name: string;
    differences: int;
    p: Dom_html.paragraphElement Js.t;
  }

  let on_done callback =
    let data = OCamlStandard.Array.make tests_count None
    and count = ref 0 in
    T.tests
    |> Li.iter_i ~f:(fun ~i {T.name; width; height; draw} ->
      let cairo_image = Dom_html.createImg Dom_html.document in
      cairo_image##.src := (Js.string (Frmt.apply "Tests/Drawing/Cairo/%s.png" name));
      cairo_image##.onload := (Dom.handler (fun _ ->
        let cairo_canvas = Dom_html.createCanvas Dom_html.document in
        cairo_canvas##.width := width;
        cairo_canvas##.height := height;
        let cairo_context = cairo_canvas##getContext Dom_html._2d_ in
        cairo_context##drawImage cairo_image 0. 0.;
        let cairo_data = cairo_context##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

        let jsooc_canvas = Dom_html.createCanvas Dom_html.document in
        jsooc_canvas##.width := width;
        jsooc_canvas##.height := height;
        draw (JsOfOCairo.create jsooc_canvas);
        let jsooc_data = (jsooc_canvas##getContext Dom_html._2d_)##getImageData 0. 0. (Fl.of_int width) (Fl.of_int height) in

        let diff_canvas = Dom_html.createCanvas Dom_html.document in
        diff_canvas##.width := width;
        diff_canvas##.height := height;
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

        let p = Dom_html.createP Dom_html.document in
        (* p##.textContent := Js.some (Js.string (Frmt.apply "Some info for %s" name)); *)
        Dom.appendChild p cairo_canvas;
        Dom.appendChild p jsooc_canvas;
        Dom.appendChild p diff_canvas;

        data.(i) <- Some {name; differences; p};
        IntRef.increment count;
        if !count = Li.size T.tests then begin
          data
          |> Li.of_array
          |> Li.filter_map ~f:identity
          |> callback
        end;
        Js._true
      ));
    )
end

let drawing_tests_name = "Drawing tests on JsOfOCairo"

let drawing_test_ps = ref StrSoMap.empty

let result_to_element result =
  let rec aux is_in_drawing_tests = function
    | Result.Single {label; status} ->
      let li = Dom_html.createLi Dom_html.document in
      let p = Dom_html.createP Dom_html.document in
      li##.className := Js.string (if status = Result.Status.Success then "success" else "failure");
      p##.textContent := Js.some (Js.string (Frmt.apply "%s: %s" label (Result.Status.to_string status)));
      Dom.appendChild li p;
      if is_in_drawing_tests then Dom.appendChild li (StrSoMap.get !drawing_test_ps ~k:label);
      li
    | Result.Group {name; children; counts={Result.Counts.successes; failures; errors}} ->
      let li = Dom_html.createLi Dom_html.document in
      let p = Dom_html.createP Dom_html.document in
      li##.className := Js.string (if failures + errors = 0 then "success" else "failure");
      p##.textContent := Js.some (Js.string (Frmt.apply "%s: (successes: %i, failures: %i, errors: %i)" name successes failures errors));
      Dom.appendChild li p;
      let ul = Dom_html.createUl Dom_html.document in
      Dom.appendChild li ul;
      children
      |> Li.iter ~f:(fun child ->
        Dom.appendChild ul (aux (name = drawing_tests_name) child)
      );
      li
  in
  let ul = Dom_html.createUl Dom_html.document in
  Dom.appendChild ul (aux false result);
  ul

let () = DrawingTests.on_done (fun drawing_tests ->
  let test =
    "Tests in browser" >:: [
      Tests.Unit.test;
      (
        let module T = Tests.Universal.Make(CairoMock)(struct
          let name = "CairoMock"

          let degraded = false

          let create = CairoMock.create
        end) in
        T.test
      );
      drawing_tests_name >:: (
        drawing_tests
        |> Li.map ~f:(fun {DrawingTests.name; differences; p} ->
          drawing_test_ps := StrSoMap.replace !drawing_test_ps ~k:name ~v:p;
          name >: (lazy (
            check_int ~expected:0 differences
          ))
        )
      );
    ]
  in
  let result = Test.run test in
  Dom.appendChild (Dom_html.getElementById "tests_in_browser") (result_to_element result);
  ignore (Js.Unsafe.eval_string {|
    jQuery("#tests_in_browser ul ul ul").hide();
    jQuery("#tests_in_browser p").click(function() {
      jQuery(this).parent().children("ul").slideToggle();
    });
  |});
)
