open InfiniteJest
open InfiniteJest.Test

(** 
   Sequence is a abstract type in the sense that we need to have a concrete 
   container to test out its functionality, here we picked Barray
*)

let yield (x:' a Sequence.t) : ('a option * 'a Sequence.t) = 
  let open Sequence in match x () with 
  | Nil -> (None, empty)
  | Cons (a, r) -> (Some a, r)

let suite =
  describe "Sequence" (fun () -> [
        test "Barray.toSequence" (fun () ->
            let s0 = Barray.toSequence [|1; 2; 3|] in
            let (a0, s1) = yield s0 in 
            let (a1, s2) = yield s1 in
            let (a2, _) = yield s2 in
            Expect.toEqual [a0; a1; a2] [Some 1; Some 2; Some 3]
          );
        test "Barray.empty" (fun () ->
            let open Sequence in
            let s = Barray.toSequence [||] in
            Expect.toEqual (s ()) (empty ())
          );
        test "Barray toSequence fromSequence invertability" (fun () ->
            [|1 ; 2; 3|] |> Barray.toSequence
            |> Barray.fromSequence
            |> Expect.toEqual [|1; 2; 3|]
          );
        test "Sequence.iter" (fun () ->
            let l = ref [] in
            [|1; 2; 3|] |> Barray.toSequence
            |> (fun iter -> Sequence.iter (fun x -> l := !l @ [x]) iter; !l)
            |> Expect.toEqual [1; 2; 3]
          );
        test "Sequence.map" (fun () ->
            [|1; 2; 3|] |> Barray.toSequence
            |> Sequence.map (fun x -> x * 2)
            |> Barray.fromSequence
            |> Expect.toEqual [| 2; 4; 6 |]
          );
        test "Sequence.filter" (fun () -> 
            [|1; 2; 3|] |> Barray.toSequence
            |> Sequence.filter (fun x -> x > 2)
            |> Barray.fromSequence
            |> Expect.toEqual [| 3 |]
          );
        test "Sequence.filterMap" (fun () -> 
            [|'1'; '2'; '3'; 'a'|] |> Barray.toSequence
            |> Sequence.filterMap (fun x -> Bchar.getDigit x)                
            |> Barray.fromSequence
            |> Expect.toEqual [|1; 2; 3|]
          );
        test "Sequence.flatMap" (fun () -> 
            [|1; 2; 3|] |> Barray.toSequence
            |> Sequence.flatMap (fun x -> Sequence.return @@ 2 * x)
            |> Barray.fromSequence
            |> Expect.toEqual [|2; 4; 6|]
          )
      ])