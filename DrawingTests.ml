(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module Make(C: Context.S) = struct
    type test = {name: string; width: int; height: int; draw: C.context -> unit}

    let make name width height draw = {name; width; height; draw}

    let tests = [
        make "move-line_to stroke" 100 100 (fun ctx ->
            C.move_to ctx ~x:10. ~y:10.;
            C.line_to ctx ~x:50. ~y:90.;
            C.line_to ctx ~x:90. ~y:10.;
            C.move_to ctx ~x:20. ~y:40.;
            C.line_to ctx ~x:80. ~y:60.;
            C.stroke ctx;
        );
        make "set-get_line_width" 100 40 (fun ctx ->
            (* set width before, during and after path *)
            C.set_line_width ctx 4.;
            C.move_to ctx ~x:10. ~y:10.;
            C.line_to ctx ~x:90. ~y:10.;
            C.stroke ctx;

            C.set_line_width ctx 12.;
            C.move_to ctx ~x:10. ~y:20.;
            C.line_to ctx ~x:50. ~y:20.;
            C.set_line_width ctx 1.;
            C.line_to ctx ~x:90. ~y:20.;
            C.stroke ctx;

            C.move_to ctx ~x:10. ~y:30.;
            C.line_to ctx ~x:90. ~y:30.;
            C.set_line_width ctx 6.;
            C.stroke ctx;
        );
        make "arc stroke_preserve fill" 100 100 (fun ctx ->
            C.arc ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:5.;
            C.set_line_width ctx 10.;
            C.stroke_preserve ctx;
            C.fill ctx;
        );
        make "arc_negative stroke fill_preserve" 100 100 (fun ctx ->
            C.arc_negative ctx ~x:50. ~y:50. ~r:40. ~a1:0. ~a2:4.;
            C.set_line_width ctx 10.;
            C.fill_preserve ctx;
            C.stroke ctx;
        );
    ]
end
