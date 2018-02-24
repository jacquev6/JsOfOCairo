module Backend = Backend

include Backend

type context = {
  mutable calls: string list;
}

let create () = {
  calls = [];
}

let call context =
  Printf.ksprintf (fun call ->
    context.calls <- call::context.calls
  )

let calls {calls} =
  calls


let move_to context ~x ~y =
  call context "move_to ~x:%.2f ~y:%.2f" x y
