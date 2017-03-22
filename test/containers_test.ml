open Containers
open Test

(* Int64 module tests *)

(* Intentionally not tested as Int64 module is just a wrapper around stdlib, this should be part of BS testing *)


(* Map module tests *)
module IntMap = Map.Make(Int64)

let to_int64 i = match Int64.of_int i  with
        | Some x -> x
        | None -> raise (Failure "Corrupted implementation of Int64.of_int")

let map0 = IntMap.add (to_int64 1) "one" IntMap.empty

let () = test_with_msg "map.get" (fun _ ->
    let x = IntMap.get (to_int64 1) map0 in match x with
    | Some str -> str = "one"
    | None -> false
)

let () = test_with_msg "map.get" (fun _ ->
    let x = IntMap.get (to_int64 2) map0 in match x with
    | Some _ -> false
    | None -> true
)

let () = test_with_msg "map.getor" (fun _ ->
    let x = IntMap.getOr (to_int64 1) map0 ~default:"default" in x = "one"
)

let () = test_with_msg "map.getor" (fun _ ->
    let x = IntMap.getOr (to_int64 2) map0 ~default:"default" in x = "default"
)

let () = test_with_msg "map.update" (fun _ ->
    let map1 = IntMap.update (to_int64 2) (function _ -> None) map0 in
        IntMap.get (to_int64 1) map1 = None
)

let () = test_with_msg "map.update" (fun _ ->
    let map1 = IntMap.update (to_int64 1) (function x -> match x with 
    | Some _ -> Some "two"
    | None -> None) map0 in IntMap.get (to_int64 1) map1 = Some "two"
)