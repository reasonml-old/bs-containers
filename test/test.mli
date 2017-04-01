type t

val test: string -> (t -> unit) -> unit
val test_: (t -> unit) -> unit

external pass: t -> 'a = "" [@@bs.send]
external fail: t -> 'a = "" [@@bs.send]
external deepEqual: t -> 'a -> 'a -> unit = "deepEqual" [@@bs.send]

external plan: t -> int -> unit = "" [@@bs.send]
