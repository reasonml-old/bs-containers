type t

external ava:< 
    test : string -> (t -> unit) -> unit[@bs.meth]
> Js.t = "ava" [@@bs.module]

let test = fun name run -> ava##test name run

let test_ = fun run -> ava##test "" run

external deepEqual: t -> 'a -> 'a -> unit = "deepEqual" [@@bs.send]

external pass: t -> unit -> unit = "" [@@bs.send]

external fail: t -> unit -> unit = "" [@@bs.send]

let () = test "dummy" (fun t ->
    deepEqual t 1 1;
    pass t ()
)