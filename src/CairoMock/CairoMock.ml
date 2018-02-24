open StdLabels

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

let run_tests () =
  let test f expected =
    let c = create () in
    ignore (f c);
    let actual = calls c in
    if actual <> expected then begin
      Printf.printf "Got:\n  %s\ninstead of:\n  %s\n" (String.concat ~sep:"\n  " actual) (String.concat ~sep:"\n  " expected);
      exit 1;
    end
  in
  begin
    test (fun c -> move_to c ~x:4.05 ~y:2.957) ["move_to ~x:4.05 ~y:2.96"];
  end
