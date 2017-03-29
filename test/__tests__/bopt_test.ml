open Test
open Bopt

let _ =
  test "make" (fun t ->
    deepEqual t (make 42) (Some 42))