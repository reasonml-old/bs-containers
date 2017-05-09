open InfiniteJest
open InfiniteJest.Test
open Barray

let suite =
  describe "Barray" (fun () -> [
        test "Barray.empty" (fun () ->
            Barray.empty |> Expect.toEqual [||]
          );
        test "Barray.isEmpty - true" (fun () ->
            [||] |> Barray.isEmpty 
            |> Expect.toEqual true
          );
        test "Barray.isEmpty - false" (fun () ->
            [|1; 2; 3|] |> Barray.isEmpty
            |> Expect.toEqual false
          );
        test "Barray.make - empty" (fun () -> 
            Barray.make 0 0 |> Expect.toEqual [||]
          );
        test "Barray.make" (fun () ->
            Barray.make 3 1 |> Expect.toEqual [| 1; 1; 1|]
          );
        test "Barray.make - large" (fun () -> 
            Barray.make 1000 1 |> Barray.length 
            |> Expect.toEqual 1000
          );
        test "Barray.make - invalid" (fun () ->
            Expect.toRaise (fun () -> Barray.make (~-1) 1) 
          );
        test "Barray.make - invalid2" (fun () ->
            Expect.toRaise (fun () -> Barray.make (Sys.max_array_length + 1) 1) 
          );
        test "Barray.init" (fun () -> 
            Barray.init 1000 (fun _ -> 1) |> Expect.toEqual (Barray.make 1000 1)
          );
        test "Barray.init empty" (fun () ->
            Barray.init 0 (fun _ -> 1) |> Expect.toEqual [||]
          );
        test "Barray.init invalid" (fun () ->
            Expect.toRaise(fun () -> Barray.init (~-1) (fun _ -> 1))
          );
        test "Barray.get" (fun () ->
            [||] |> Barray.get 0
            |> Expect.toEqual None
          );
        test "Barray.get - 1" (fun () ->
            [|1; 2; 3|] |> Barray.get 1
            |> Expect.toEqual (Some 2)
          );
        test "Barray.get - 2" (fun () ->
            [|1; 2; 3|] |> Barray.get 3
            |> Expect.toEqual (None)
          );
        test "Barray.set" (fun () ->
            let arr = [|1; 2; 3|] in
            let b = Barray.set 1 3 arr in
            Expect.toEqual (b, arr) (true, [|1; 3; 3|])
          );
        test "Barray.set - invalid" (fun () ->
            let arr = [|1; 2; 3|] in
            let b = Barray.set (~-1) 3 arr in
            Expect.toEqual (b, arr) (false, [|1; 2; 3|])
          );
        test "Barray.getOrRaise" (fun () ->
            [|1; 2; 3|] |> Barray.getOrRaise 1
            |> Expect.toEqual 2
          );
        test "Barray.getOrRaise - invalid" (fun () ->
            Expect.toRaise (fun () -> Barray.getOrRaise 5 [|1; 2; 3|])
          );
        test "Barray.setOrRaise" (fun () ->
            let x = [|1; 2; 3|] in
            Barray.setOrRaise 1 1 x;
            Expect.toEqual x [|1; 1; 3|]
          );
        test "Barray.setOrRaise - invalid" (fun () ->
            Expect.toRaise (fun () -> Barray.setOrRaise 5 0 [|1; 2; 3|])
          );
        test "Barray.equals" (fun () -> 
            [| |] |> Barray.equals (=) [| |]
            |> Expect.toEqual true
          );
        test "Barray.equals - false 1" (fun () -> 
            [| |] |> Barray.equals (=) [| 1; 2; 3 |]
            |> Expect.toEqual false
          );
        test "Barray.equals - false 2" (fun () -> 
            [| 1 |] |> Barray.equals (=) [| 1 |]
            |> Expect.toEqual true
          );
        test "Barray.compare" (fun () -> 
            [||] |> Barray.compare (Comparator.make Pervasives.compare) [||] 
            |> Expect.toEqual Ordering.Equal  
          );
        test "Barray.compare - 1" (fun () -> 
            [||] |> Barray.compare (Comparator.int) [| 1; 2; 3 |]
            |> Expect.toEqual (Ordering.Greater)
          );
        test "Barray.compare - 2" (fun () -> 
            [|1; 2; 3|] |> Barray.compare (Comparator.int) [| 1; 2; 3 |]
            |> Expect.toEqual (Ordering.Equal)
          );
        test "Barray.compare - 3" (fun () -> 
            [|1; 2; 3; 4|] |> Barray.compare (Comparator.int) [| 1; 2; 3 |]
            |> Expect.toEqual (Ordering.Less)
          );
        test "Barray.compare - 4" (fun () -> 
            [|1; 2; 4|] |> Barray.compare (Comparator.int) [| 1; 2; 3 |]
            |> Expect.toEqual (Ordering.Less)
          );
        test "Barray.concat" (fun () ->
            [] |> Barray.concat
            |> Expect.toEqual [||]
          );
        test "Barray.concat - 1" (fun () -> 
            [[|1|]; [|2|]; [|3|]] |> Barray.concat
            |> Expect.toEqual [|1; 2; 3|]
          );
        test "Barray.append - empty" (fun () -> 
            [|1; 2; 3|] |> Barray.append [| |] 
            |> Expect.toEqual [| 1; 2; 3|]
          );
        test "Barray.append" (fun () -> 
            [|1; 2; 3|] |> Barray.append [| 4 |] 
            |> Expect.toEqual [| 4; 1; 2; 3|]
          );
        test "Barray.slice" (fun () -> 
            [||] |> slice 0 0
            |> Expect.toEqual (Some [||])
          );
        test "Barray.slice 1" (fun () ->
            [|1|] |> slice 0 1
            |> Expect.toEqual (Some [|1|])
          );
        test "Barray.slice 2" (fun () ->
            [|1|] |> slice 0 0
            |> Expect.toEqual (Some [||])
          );
        test "Barray.slice 3" (fun () -> 
            [|1; 2; 3|] |> slice 0 100
            |> Expect.toEqual None
          );
        test "Barray.copy" (fun () ->
            [|1; 2; 3|] |> Barray.copy 
            |> Expect.toEqual [|1; 2; 3|]
          );
        test "Barray.copy - physically not equal" (fun () ->
            let a = [| 1; 2; 3|] in 
            let b = Barray.copy [| 1; 2; 3|] in 
            Expect.toBeFalse (a == b)
          );
        test "Barray.toList" (fun () ->
            [||] |> Barray.toList
            |> Expect.toEqual []
          );
        test "Barray.toList" (fun () ->
            [|1; 2; 3|] |> Barray.toList
            |> Expect.toEqual [1; 2; 3]
          );
        test "Barray.fromList" (fun () ->
            [] |> Barray.fromList
            |> Expect.toEqual [||]
          );
        test "Barray.fromList" (fun () ->
            [1; 2; 3] |> Barray.fromList
            |> Expect.toEqual [|1; 2; 3|]
          );
        (** Sequence related functions are tested in sequence_test *)
        test "Barray.map" (fun () -> 
            [|1; 2; 3|] |> Barray.map (fun x -> x * 2) 
            |> Expect.toEqual [|2; 4; 6|]
          );
        test "Barray.map empty" (fun () ->
            [||] |> Barray.map (fun x -> x) 
            |> Expect.toEqual [||]
          );
        test "Barray.mapi" (fun () ->
            [|1; 2; 3|] |> Barray.mapi (fun i x -> i + x)
            |> Expect.toEqual [|1; 3; 5|]
          );
        test "Barray.mapi empty" (fun () -> 
            [||] |> Barray.mapi (fun _ x -> x) 
            |> Expect.toEqual [||]
          );
        test "Barray.map2 empty empty" (fun () ->
            let c = Barray.map2 (fun _ _ -> 1) [||] [||] 
            in Expect.toEqual (Some [||]) c
          );
        test "Barray.map2 empty noempty" (fun () ->
            let c = Barray.map2 (fun _ _ -> 1) [||] [|1; 2; 3|]
            in Expect.toEqual None c
          );
        test "Barray.map2" (fun () -> 
            let c = Barray.map2 (fun x y -> Bchar.getDigitOrRaise x + y) [|'1'; '2'; '3'|] [|1; 2; 3|]
            in Expect.toEqual (Some [|2; 4; 6|]) c
          ); 
      ])