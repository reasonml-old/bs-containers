open InfiniteJest
open InfiniteJest.Test
open Containers

(* Int64 module tests *)

(* Intentionally not tested as Int64 module is just a wrapper around stdlib, this should be part of BS testing *)

(* Map module tests *)
module IntMap = Map.Make(Int64)
open IntMap

let to_int64 i = match Int64.fromInt i  with
  | Some x -> x
  | None -> raise (Failure "Corrupted implementation of Int64.of_int")

let key1 = to_int64 1
let key2 = to_int64 2
let key3 = to_int64 3
let map0 = add key1 "one" empty

let suite = 
  describe "Bmap" (fun () -> [
    test "size empty" (fun () -> 
      empty |> size
            |> Expect.toEqual 0);
    test "size map0" (fun () -> 
      map0 |> size
           |> Expect.toEqual 1);

    test "get - success" (fun () -> 
      map0 |> get key1
           |> Expect.toEqual (Some "one"));
    test "get - fail" (fun () -> 
      map0 |> get key2
           |> Expect.toEqual None);

    test "getOr - success" (fun () -> 
      map0 |> getOr key1 ~default:"default"
           |> Expect.toEqual "one");
    test "getOr - fail" (fun () -> 
      map0 |> getOr key2 ~default:"default"
           |> Expect.toEqual "default");

    test "update - does not touch other cells" (fun () -> 
      let map1 = map0 |> update key2 (function _ -> None) in
      map1 |> get key1
           |> Expect.toEqual (Some "one"));
    test "update - Some -> Some" (fun () -> 
      let map1 = map0 |> update key1 (function | Some _ -> Some "two" | v -> v) in
      map1 |> get key1
           |> Expect.toEqual (Some "two"));
    test "update - None -> Some" (fun () -> 
      let map1 = map0 |> update key2 (function | None -> Some "two" | v -> v) in
      map1 |> get key2
           |> Expect.toEqual (Some "two"));
    test "update - Some -> None" (fun () -> 
      let map1 = map0 |> update key1 (function _ -> None) in
      map1 |> get key1
           |> Expect.toEqual None);

    test "update - Some -> None" (fun () -> 
      let map1 = singleton key2 "two" in
      let merged = mergeSafe ~f:(fun _ -> function
        | `Both (_, _) -> assert false
        | `Left _ -> Some "left"
        | `Right _ -> Some "right") map0 map1 in
      (merged |> get key1, merged |> get key2)
        |> Expect.toEqual (Some "left", Some "right"));

    test "fromList" (fun () ->
      let map1 = fromList [
        key1, "one";
        key2, "two";
        key3, "three"
      ] in
      map1 |> get key3
           |> Expect.toEqual (Some "three")
    );

    test "toList" (fun () ->
      map0 |> toList
           |> Expect.toEqual [key1, "one"]);

    test "fromSeq" (fun () ->
      let map1 = fromSeq (Sequence.singleton (key1, "hello")) in
      map1 |> size
           |> Expect.toEqual 1);

    test "toSeq" (fun () ->
      map0 |> toSeq |> Sequence.to_list
           |> Expect.toEqual (map0 |> toList));

    test "keysList" (fun () ->
      map0 |> keysList
           |> Expect.toEqual [key1]);

    test "valuesList" (fun () ->
      map0 |> valuesList
           |> Expect.toEqual ["one"]);
  ])