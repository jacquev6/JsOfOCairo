val drawing_tests: string list

module Make(X: sig
  val title: string

  module C: CairoMock.S

  module N: sig
    val name: string
    val create: unit -> C.context
    val degraded: bool
  end

  module DrawingTest(T: sig
    type t = {name: string; width: int; height: int; draw: C.context -> unit}
  end): sig
    val run: T.t -> unit
  end
end): sig
  val test: General.Testing.Test.t
end
