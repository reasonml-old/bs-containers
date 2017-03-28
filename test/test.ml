type t

external ava:< 
test : string -> (t -> unit) -> unit[@bs.meth]
> Js.t = "ava" [@@bs.module]

let test = fun run -> ava##test "" run

let testWithName = fun name run -> ava##test name run

external deepEqual: t -> 'a -> 'a -> unit = "deepEqual" [@@bs.send]

external pass: t -> unit -> unit = "" [@@bs.send]

external fail: t -> unit -> unit = "" [@@bs.send]

let () = testWithName "dummy" (fun t ->
    deepEqual t 1 1;
    pass t ()
)