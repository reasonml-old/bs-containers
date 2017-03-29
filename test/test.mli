(*Ideally we should have a unified interface of testing framework, I am still thinking about how to abstract away the difference between JS testing and OUnit.*)
type t

val test: string -> (t -> unit) -> unit
val test_: (t -> unit) -> unit

val pass: t -> unit -> unit
val fail: t -> unit -> unit
val deepEqual: t -> 'a -> 'a -> unit