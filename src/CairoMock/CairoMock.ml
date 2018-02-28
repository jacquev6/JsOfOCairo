open StdLabels

module Backend = Backend

include Backend

module State = struct
  type t = {
    transformation: Matrix.t;
  }
end

type context = {
  mutable calls: string list;
  mutable states: State.t list;
  points: Points.t;
}

let create () = {
  calls = [];
  states = [
    {
      transformation = Matrix.init_identity ();
    };
  ];
  points = Points.create ();
}

let call context =
  Printf.ksprintf (fun call ->
    context.calls <- call::context.calls
  )

let calls {calls; _} =
  calls

let state {states; _} =
  List.hd states


let move_to context ~x ~y =
  call context "move_to ~x:%.2f ~y:%.2f" x y;
  let transformation = (state context).transformation in
  Points.set_start context.points ~transformation ~x ~y;
  Points.set_current_from_start context.points

let line_to context ~x ~y =
  call context "line_to ~x:%.2f ~y:%.2f" x y;
  let transformation = (state context).transformation in
  Points.set_start_if_none context.points ~transformation ~x ~y;
  Points.set_current context.points ~transformation ~x ~y
