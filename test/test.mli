(*Ideally we should have a unified interface of testing framework, I am still thinking about how to abstract away the difference between JS testing and OUnit.*)
type ava

type t

val test: (t -> unit) -> unit

val test_with_msg: string -> (t -> unit) -> unit

val truthy: t -> 'a -> unit

val falsy: t -> 'a -> unit

