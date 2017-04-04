open Test

module C = Containers
(* Int64 module tests *)

(* Intentionally not tested as Int64 module is just a wrapper around stdlib, this should be part of BS testing *)

(* Map module tests *)
module IntMap = C.Map.Make(C.Int64)

let to_int64 i = match C.Int64.fromInt i  with
  | Some x -> x
  | None -> raise (Failure "Corrupted implementation of Int64.of_int")

let map0 = IntMap.add (to_int64 1) "one" IntMap.empty

let () = test "size empty" (fun t -> 
    let x = IntMap.size IntMap.empty in
    deepEqual t x 0
  )

let () = test "size singleton" (fun t -> 
    let x = IntMap.size map0 in
    deepEqual t x 1
  )

let () = test "get succsess" (fun t ->
    deepEqual t (IntMap.get (to_int64 1) map0) (Some "one")
  )

let () = test "get" (fun t ->
    deepEqual t (IntMap.get (to_int64 2) map0) None
  )

let () = test "getor" (fun t ->
    let x = IntMap.getOr (to_int64 1) map0 ~default:"default" in deepEqual t x "one"
  )

let () = test "getor" (fun t ->
    let x = IntMap.getOr (to_int64 2) map0 ~default:"default" in deepEqual t x "default"
  )

let () = test "update_0" (fun t ->
    let map1 = IntMap.update (to_int64 2) (function _ -> None) map0 in
    deepEqual t (IntMap.get (to_int64 1) map1) (Some "one")
  )

let () = test "update_1" (fun t ->
    let map1 = IntMap.update (to_int64 1) (function
        | Some _ -> Some "two"
        | None -> None) map0 in 
    deepEqual t (IntMap.get (to_int64 1) map1) (Some "two"); 
    deepEqual t (IntMap.size map1) 1
  )

let () = test "update_2" (fun t ->
    let map1 = IntMap.update (to_int64 2) (function x -> match x with 
        | Some _ -> None
        | None -> Some "two"
      ) map0 in deepEqual t (IntMap.get (to_int64 2) map1) (Some "two"); deepEqual t (IntMap.size map1) 2
  )

let () = test "update_3" (fun t ->
    let map1 = IntMap.update (to_int64 1) (function _ -> None) map0
    in deepEqual t (IntMap.size map1) 0
  )

let () = test "mergeSafe_0" (fun t ->
    let map1 = IntMap.singleton (to_int64 2) "Hello" in
    let mapM = IntMap.mergeSafe ~f:(fun _ pre -> match pre with 
        | `Both (_, _) -> fail t
        | _ -> Some "value") map0 map1 in
    deepEqual t (IntMap.size map0) 1;
    deepEqual t (IntMap.size map1) 1; 
    deepEqual t (IntMap.size mapM) 2;
    deepEqual t (IntMap.get (to_int64 1) mapM) (Some "value");
    deepEqual t (IntMap.get (to_int64 2) mapM) (Some "value")
  )

let () = test "fromList_0" (fun t ->
    let map1 = IntMap.fromList [to_int64 1, "one";to_int64 2, "two";to_int64 3, "three"] in
    deepEqual t (IntMap.size map1) 3;
    deepEqual t (IntMap.get (to_int64 1) map1) (Some "one")
  )

let () = test "toList_0" (fun t ->
    let list0 = IntMap.toList map0 in 
    deepEqual t (List.length list0) 1
  )

let () = test "fromSeq_0" (fun t ->
    let map1 = IntMap.fromSeq (Sequence.singleton (to_int64 1, "hello")) in
    deepEqual t (IntMap.size map1) 1 
  )

let () = test "toSeq_0" (fun t ->
    let s = IntMap.toSeq map0
    in s (fun x -> let _, v = x in deepEqual t v "one")
  )

let () = test "keys" (fun t ->
    let k = IntMap.keysList map0 
    in deepEqual t k [(to_int64 1)]
  )

let () = test "values" (fun t ->
    let v = IntMap.valuesList map0 
    in deepEqual t v ["one"]
  )
