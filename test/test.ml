type ava
type exe

external t:ava = "ava" [@@bs.module]

(*external ava: ava = "ava" [@@bs.module];;*)

external _test: ava -> (exe -> bool) -> unit = "test" [@@bs.send]

external _test_with_msg: ava -> string -> (exe -> bool) -> unit = "test" [@@bs.send]

(* I would love to keep this interface, but this will trigger a warning for BS *)
external _truthy: exe -> 'a -> Js.boolean = "truthy" [@@bs.send]

external _falsy: exe -> 'a -> Js.boolean = "falsy" [@@bs.send]

let test = _test t

let test_with_msg = _test_with_msg t

let truthy x a = Js.to_bool @@ _truthy x a 

let falsy x a = Js.to_bool @@ _falsy x a
