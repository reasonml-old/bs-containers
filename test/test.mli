(*Ideally we should have a unified interface of testing framework, I am still thinking about how to abstract away the difference between JS testing and OUnit.*)
type ava
type exe

val test: (exe -> bool) -> unit

val test_with_msg: string -> (exe -> bool) -> unit

val truthy: exe -> 'a -> bool 

val falsy: exe -> 'a -> bool
