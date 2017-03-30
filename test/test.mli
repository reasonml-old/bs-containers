(*Ideally we should have a unified interface of testing framework, I am still thinking about how to abstract away the difference between JS testing and OUnit.*)
type t

val test: string -> (t -> unit) -> unit
val test_: (t -> unit) -> unit

external pass: t -> unit = "" [@@bs.send]
external fail: t -> unit = "" [@@bs.send]
external deepEqual: t -> 'a -> 'a -> unit = "deepEqual" [@@bs.send]

external plan: t -> int -> unit = "" [@@bs.send]