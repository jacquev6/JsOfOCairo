(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module Make(C: CairoMock.S) = struct
  type t = {name: string; width: int; height: int; draws: (C.context -> string list) list}

  module DecoratedC = CairoMock.Decorate(C)

  let make name width height draws =
    let draws =
      draws
      |> Li.map ~f:(fun draw ->
        fun ctx ->
          let ctx = DecoratedC.create ctx in
          draw ctx;
          DecoratedC.calls ctx
      )
    in
    {name; width; height; draws}

  let limitations = DecoratedC.([
    make "arc_more_than_2pi" 100 100 [fun ctx ->
      arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:7.;
      ignore (Path.get_current_point ctx);
      line_to ctx ~x:50. ~y:50.;
      stroke ctx
    ];
    make "font_extents" 100 10 [fun ctx ->
      ignore (font_extents ctx)
    ];
    make "text_extents" 100 10 [fun ctx ->
      ignore (text_extents ctx "Hello")
    ];
    make "reuse_canvas_transformation" 100 60 [
      scale ~x:2. ~y:3.;
      (fun ctx ->
        move_to ctx ~x:10. ~y:10.;
        line_to ctx ~x:40. ~y:10.;
        stroke ctx);
    ];
    make "reuse_canvas_line_cap" 100 60 [
      (fun ctx -> set_line_cap ctx ROUND);
      (fun ctx ->
        set_line_width ctx 20.;
        move_to ctx ~x:30. ~y:30.;
        line_to ctx ~x:70. ~y:30.;
        stroke ctx);
    ];
  ])
end
