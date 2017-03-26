type ava
type t

external ava: ava = "ava" [@@bs.module]

external _test: ava -> (t -> unit) -> unit = "test" [@@bs.send]

external _test_with_msg: ava -> string -> (t -> unit) -> unit = "test" [@@bs.send]

let test = function run -> _test ava run

let test_with_msg = function run -> _test_with_msg ava run

external truthy: t -> 'a -> unit = "truthy" [@@bs.send]

external falsy: t -> 'a -> unit = "falsy" [@@bs.send]
