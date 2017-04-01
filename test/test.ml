type t

external ava:'a Js.t = "ava" [@@bs.module]

let test = fun name run -> ava##test name run
let test_ = fun run -> ava##test "" run

external pass: t -> 'a = "" [@@bs.send]
external fail: t -> 'a = "" [@@bs.send]
external deepEqual: t -> 'a -> 'a -> unit = "deepEqual" [@@bs.send]

external plan: t -> int -> unit = "" [@@bs.send]

let () = test "dummy" (fun t ->
  deepEqual t 1 1;
  pass t
)

