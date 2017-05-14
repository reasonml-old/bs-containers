open InfiniteJest
open InfiniteJest.Test

(** 
   Bsequence is a abstract type in the sense that we need to have a concrete 
   container to test out its functionality, here we picked Barray
*)

let yield (x:' a Bsequence.t) : ('a option * 'a Bsequence.t) = 
  let open Bsequence in match x () with 
  | Nil -> (None, empty)
  | Cons (a, r) -> (Some a, r)

let suite =
  describe "Bsequence" (fun () -> [
        test "Barray.toBsequence" (fun () ->
            let s0 = Barray.toSequence [|1; 2; 3|] in
            let (a0, s1) = yield s0 in 
            let (a1, s2) = yield s1 in
            let (a2, _) = yield s2 in
            Expect.toEqual [a0; a1; a2] [Some 1; Some 2; Some 3]
          );
        test "Barray.empty" (fun () ->
            let open Bsequence in
            let s = Barray.toSequence [||] in
            Expect.toEqual (s ()) (empty ())
          );
        test "Barray toBsequence fromBsequence invertability" (fun () ->
            [|1 ; 2; 3|] |> Barray.toSequence
            |> Barray.fromSequence
            |> Expect.toEqual [|1; 2; 3|]
          );
        test "Bsequence.iter" (fun () ->
            let l = ref [] in
            [|1; 2; 3|] |> Barray.toSequence
            |> (fun iter -> Bsequence.iter (fun x -> l := !l @ [x]) iter; !l)
            |> Expect.toEqual [1; 2; 3]
          );
        test "Bsequence.map" (fun () ->
            [|1; 2; 3|] |> Barray.toSequence
            |> Bsequence.map (fun x -> x * 2)
            |> Barray.fromSequence
            |> Expect.toEqual [| 2; 4; 6 |]
          );
        test "Bsequence.filter" (fun () -> 
            [|1; 2; 3|] |> Barray.toSequence
            |> Bsequence.filter (fun x -> x > 2)
            |> Barray.fromSequence
            |> Expect.toEqual [| 3 |]
          );
        test "Bsequence.filterMap" (fun () -> 
            [|'1'; '2'; '3'; 'a'|] |> Barray.toSequence
            |> Bsequence.filterMap (fun x -> Bchar.getDigit x)                
            |> Barray.fromSequence
            |> Expect.toEqual [|1; 2; 3|]
          );
        test "Bsequence.flatMap" (fun () -> 
            [|1; 2; 3|] |> Barray.toSequence
            |> Bsequence.flatMap (fun x -> Bsequence.return @@ 2 * x)
            |> Barray.fromSequence
            |> Expect.toEqual [|2; 4; 6|]
          )
      ])