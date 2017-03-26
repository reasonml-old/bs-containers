open Containers
open Test

(* Dummy tests for test runners *)

let () = test (fun t ->
    truthy t true;
    falsy t false
)

(* Int64 module tests *)

(* Intentionally not tested as Int64 module is just a wrapper around stdlib, this should be part of BS testing *)


(* Map module tests *)
module IntMap = Map.Make(Int64)

let to_int64 i = match Int64.of_int i  with
        | Some x -> x
        | None -> raise (Failure "Corrupted implementation of Int64.of_int")

let map0 = IntMap.add (to_int64 1) "one" IntMap.empty

let () = test_with_msg "map.size" (fun t -> 
    let x = IntMap.size IntMap.empty in
    truthy t (x = 0)
)

let () = test_with_msg "map.size" (fun t -> 
    let x = IntMap.size map0 in
    truthy t (x = 1)
)

let () = test_with_msg "map.get" (fun t ->
    let x = IntMap.get (to_int64 1) map0 in match x with
    | Some str -> truthy t (str = "one")
    | None -> truthy t false
)

let () = test_with_msg "map.get" (fun t ->
    let x = IntMap.get (to_int64 2) map0 in match x with
    | Some _ -> truthy t false
    | None -> truthy t true
)

let () = test_with_msg "map.getor" (fun t ->
    let x = IntMap.getOr (to_int64 1) map0 ~default:"default" in truthy t (x = "one")
)

let () = test_with_msg "map.getor" (fun t ->
    let x = IntMap.getOr (to_int64 2) map0 ~default:"default" in truthy t (x = "default")
)

let () = test_with_msg "map.update_0" (fun t ->
    let map1 = IntMap.update (to_int64 2) (function _ -> None) map0 in
        truthy t (IntMap.get (to_int64 1) map1 = Some "one")
)

let () = test_with_msg "map.update_1" (fun t ->
    let map1 = IntMap.update (to_int64 1) (function x -> match x with 
    | Some _ -> Some "two"
    | None -> None) map0 in truthy t (IntMap.get (to_int64 1) map1 = Some "two" && IntMap.size map1 = 1)
)

let () = test_with_msg "map.update_2" (fun t ->
    let map1 = IntMap.update (to_int64 2) (function x -> match x with 
    | Some _ -> None
    | None -> Some "two"
    ) map0 in truthy t (IntMap.get (to_int64 2) map1 = Some "two" && IntMap.size map1 = 2)
)

let () = test_with_msg "map.update_3" (fun t ->
    let map1 = IntMap.update (to_int64 1) (function _ -> None) map0
    in truthy t (IntMap.size map1 = 0)
)

let () = test_with_msg "map.mergeSafe_0" (fun t ->
    let map1 = IntMap.singleton (to_int64 2) "Hello" in
    let mapM = IntMap.mergeSafe ~f:(fun _ pre -> match pre with 
    | `Both (_, _) -> truthy t false; None
    | _ -> Some "value") map0 map1 in
    truthy t (
        IntMap.size map0 = 1 &&
        IntMap.size map1 = 1 && 
        IntMap.size mapM = 2 &&
        IntMap.get (to_int64 1) mapM = Some "value" &&
        IntMap.get (to_int64 2) mapM = Some "value"
    )
)

let () = test_with_msg "map.ofList_0" (fun t ->
    let map1 = IntMap.ofList [to_int64 1, "one";to_int64 2, "two";to_int64 3, "three"] in
    truthy t (IntMap.size map1 = 3);
    truthy t (IntMap.get (to_int64 1) map1 = Some "one")
)

let () = test_with_msg "msg.toList_0" (fun t ->
    let list0 = IntMap.toList map0 in 
    truthy t (List.length list0 = 1)
)