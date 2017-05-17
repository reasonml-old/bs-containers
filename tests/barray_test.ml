open InfiniteJest
open InfiniteJest.Test

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
            test "Barray.makeWithInit" (fun () -> 
                Barray.makeWithInit 1000 (fun _ -> 1) |> Expect.toEqual (Barray.make 1000 1)
            );
            test "Barray.makeWithInit empty" (fun () ->
                Barray.makeWithInit 0 (fun _ -> 1) |> Expect.toEqual [||]
            );
            test "Barray.makeWithInit invalid" (fun () ->
                Expect.toRaise(fun () -> Barray.makeWithInit (~-1) (fun _ -> 1))
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
                [||] |> Barray.slice ~start_:0 ~end_:0
                |> Expect.toEqual (Some [||])
            );
            test "Barray.slice 1" (fun () ->
                [|1|] |> Barray.slice ~start_:0 ~end_:1
                |> Expect.toEqual (Some [|1|])
            );
            test "Barray.slice 2" (fun () ->
                [|1|] |> Barray.slice ~start_:0 ~end_:0
                |> Expect.toEqual (Some [||])
            );
            test "Barray.slice 3" (fun () -> 
                [|1; 2; 3|] |> Barray.slice ~start_:0 ~end_:100
                |> Expect.toEqual None
            );
            test "Barray.slice 4" (fun () ->
                [|1; 2; 3; 4|] |> Barray.slice ~start_:1 ~end_:2 
                |> Expect.toEqual (Some [| 2 |])
            );
            test "Barray.slice 5" (fun () -> 
                [|1; 2; 3; 4|] |> Barray.slice ~start_:1 ~end_:3
                |> Expect.toEqual (Some [| 2; 3 |])
            );

            test "Barray.slice 6" (fun () -> 
                [|1;2;3;4|] |> Barray.slice ~start_:0 ~end_:4
                |> Expect.toEqual (Some [|1; 2; 3; 4|])
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
            (** Sequence related functions are tested in sequence_test *)
            test "Barray.map" (fun () -> 
                [|1; 2; 3|] |> Barray.map (fun x -> x * 2) 
                |> Expect.toEqual [|2; 4; 6|]
            );
            test "Barray.map empty" (fun () ->
                [||] |> Barray.map (fun x -> x) 
                |> Expect.toEqual [||]
            );
            test "Barray.mapWithIndex" (fun () ->
                [|1; 2; 3|] |> Barray.mapWithIndex (fun i x -> i + x)
                |> Expect.toEqual [|1; 3; 5|]
            );
            test "Barray.mapWithIndex empty" (fun () -> 
                [||] |> Barray.mapWithIndex (fun _ x -> x) 
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
            test "Barray.reduce empty" (fun () -> 
                [||] |> Barray.reduce (fun acc x -> acc + x) 0 
                |> Expect.toEqual 0
            );
            test "Barray.reduce" (fun () -> 
                [|1; 2; 3|] |> Barray.reduce (fun acc x -> acc + x) 0
                |> Expect.toEqual 6
            );
            test "Barray.reduce toList" (fun () ->
                [|1; 2; 3|] |> Barray.reduce (fun acc x -> acc @ [x]) [] 
                |> Expect.toEqual [1; 2; 3]
            );
            test "Barray.reduceReversed empty" (fun () ->
                [||] |> Barray.reduceReversed (fun acc x -> acc + x) 0
                |> Expect.toEqual 0
            );
            test "Barray.reduceReversed reverse" (fun () -> 
                [|1; 2; 3|] |> Barray.reduceReversed (fun x acc -> acc @ [x]) []
                |> Expect.toEqual [3; 2; 1] 
            );
            test "Barray.reduceWithIndex empty" (fun () ->
                [||] |> Barray.reduceWithIndex (fun _ _ _ -> 0) 0
                |> Expect.toEqual 0
            );
            test "Barray.reduceWithIndex" (fun () ->
                [|1; 2; 3|] |> Barray.reduceWithIndex (fun acc index x -> acc + index + x) 0
                |> Expect.toEqual 9
            );
            test "Barray.reduceWhile empty" (fun () ->
                [||] |> Barray.reduceWhile (fun acc _ -> acc , true) 0
                |> Expect.toEqual 0
            );
            test "Barray.reduceWhile" (fun () ->
                [|1; 2; 3|] |> Barray.reduceWhile (fun acc x -> 
                    if x > 4 then (acc, true) else (acc + x, false)) 0
                |> Expect.toEqual 6
            );
            test "Barray.reduceWhile stop" (fun () ->
                [|1; 2; 3; 4|] |> Barray.reduceWhile (fun acc x -> 
                    if x >= 3 then (acc, true) else (acc + x, false)) 0
                |> Expect.toEqual 3
            );
            test "Barray.forEachWithIndex empty" (fun () ->
                let r = ref 0 in
                let () = Barray.forEachWithIndex (fun _ _ -> r := !r + 1) [||]
                in Expect.toEqual !r 0
            );
            test "Barray.forEachWithIndex" (fun () ->
                let r = ref 0 in
                let () = Barray.forEachWithIndex (fun x _ -> r := !r + x ) [|1; 2; 3|]
                in Expect.toEqual !r 6
            );
            test "Barray.blit empty empty" (fun () -> 
                let a = [||] in
                let b = [||] in
                let _ = Barray.blit ~x:a ~idx:0 ~y:b ~idy:0 ~len:0 in 
                Expect.toEqual b [||]
            );
            test "Barray.blit empty empty long" (fun () -> 
                let a = [||] in
                let b = [||] in
                Expect.toRaise (fun () -> Barray.blit ~x:a ~idx:0 ~y:b ~idy:0 ~len:100)
            );
            test "Barray.blit" (fun () -> 
                let a = [|1; 2; 3; 4|] in
                let b = [|5; 6; 7; 8|] in
                let _ = Barray.blit ~x:a ~idx:0 ~y:b ~idy:0 ~len:4 in
                Expect.toEqual b [|1; 2; 3; 4|]
            );
            test "Barray.blit self" (fun () ->
                let a = [|1; 2; 3; 4|] in
                let _ = Barray.blit ~x:a ~idx:0 ~y:a ~idy:2 ~len:2 in
                Expect.toEqual a [|1; 2; 1; 2|]
            );
            test "Barray.reverse" (fun () ->
                [|1; 2; 3; 4|] |> Barray.reverse
                |> Expect.toEqual [|4; 3; 2; 1|]
            );
            test "Barray.reverseInPlace" (fun () ->
                let a = [|1; 2; 3; 4|]
                in Barray.reverseInPlace a; Expect.toEqual a [|4; 3; 2; 1|] 
            );
            test "Barray.sort" (fun () -> 
                let a = [| 1; 4; 3; 2; 6; 5; 8; 7|] 
                in Barray.sort Comparator.int a;
                Expect.toEqual a [| 1; 2; 3; 4; 5; 6; 7; 8|]
            );
            test "Barray.sort empty" (fun () ->
                let a = [||]
                in Barray.sort Comparator.int a;
                Expect.toEqual a [||];
            );
            test "Barray.sortIndices empty" (fun () ->
                [||] |> Barray.sortIndices Comparator.int 
                |> Expect.toEqual [||]
            );
            test "Barray.sortIndices" (fun () -> 
                [|4; 2; 1; 3|] |> Barray.sortIndices Comparator.int
                |> Expect.toEqual [|2; 1; 3; 0|]
            );
            test "Barray.sortRanking empty" (fun () -> 
                [||] |> Barray.sortRanking Comparator.int
                |> Expect.toEqual [||]
            );
            test "Barray.sortRanking" (fun () -> 
                [|4; 2; 1; 3|] |> Barray.sortRanking Comparator.int
                |> Expect.toEqual [|3; 1; 0; 2|]
            );
            test "Barry.find empty" (fun () ->
                [||] |> Barray.find (fun x -> Some x)
                |> Expect.toEqual None
            );
            test "Barray.find" (fun () ->
                [|1; 2; 3; 4|] |> Barray.find (fun x -> if x == 2 then Some x else None) 
                |> Expect.toEqual (Some 2)
            );
            test "Barray.findWithIndex empty" (fun () -> 
                [||] |> Barray.findWithIndex (fun _ x -> Some (1, x))
                |> Expect.toEqual None
            );
            test "Barray.findWithIndex" (fun () ->
                [|4; 3; 2; 1; 5|] |> Barray.findWithIndex (fun i x -> if i + x = 4 then Some(x, i) else None) 
                |> Expect.toEqual @@ Some (4,0)
            );
            test "Barray.findIndex empty" (fun () -> 
                [||] |> Barray.findIndex (fun _ -> true)
                |> Expect.toEqual None
            );
            test "Barray.findIndex" (fun () ->
                [|5; 4; 3; 1; 2|] |> Barray.findIndex(fun x -> if x = 4 then true else false) 
                |> Expect.toEqual @@ Some (1, 4)
            );
            test "Barray.findIndex - not found" (fun () -> 
                [|5; 4; 3; 1; 2|] |> Barray.findIndex (fun x -> if x = 7 then true else false) 
                |> Expect.toEqual None
            );
            test "Barray.bsearch empty" (fun () -> 
                [||] |> Barray.bsearch Comparator.int 1
                |> Expect.toEqual `Empty
            );
            test "Barray.bsearch at" (fun () ->
                [|1; 2; 3; 5; 8|] |> Barray.bsearch Comparator.int 3
                |> Expect.toEqual @@ `At 2
            );
            test "Barray.bsearch just_after" (fun () ->
                [|1; 2; 3; 5; 8|] |> Barray.bsearch Comparator.int 4
                |> Expect.toEqual @@ `Just_after 2
            );
            test "Barray.bsearch all_bigger" (fun () ->
                [|1; 2; 3; 5; 8|] |> Barray.bsearch Comparator.int 0
                |> Expect.toEqual `All_bigger
            );
            test "Barray.bsearch all_lower" (fun () ->
                [|1; 2; 3; 5; 8|] |> Barray.bsearch Comparator.int 9
                |> Expect.toEqual `All_lower
            );
            test "Barray.forAll empty" (fun () ->
                [||] |> Barray.forAll (fun _ -> true)
                |> Expect.toEqual true
            );
            test "Barray.forAll" (fun () -> 
                [|1; 2; 3|] |> Barray.forAll (fun x -> x < 4)
                |> Expect.toEqual true
            );
            test "Barray.forAll false" (fun () -> 
                [|1; 2; 3|] |> Barray.forAll (fun x -> x < 2) 
                |> Expect.toEqual false
            );
            test "Barray.exists empty" (fun () ->
                [||] |> Barray.exists (fun _ -> true)
                |> Expect.toEqual false;
            );
            test "Barray.exists" (fun () -> 
                [|1; 2; 3; 4|] |> Barray.exists (fun x -> x > 2)
                |> Expect.toEqual true
            );
            test "Barray.count empty" (fun () ->
                [||] |> Barray.count (fun _ -> true)
                |> Expect.toEqual 0
            );
            test "Barray.count" (fun () -> 
                [|1; 2; 3; 4|] |> Barray.count (fun x -> x > 2)
                |> Expect.toEqual 2
            );
            test "Barray.forEach empty" (fun () ->
                let a = ref [] in
                Barray.forEach (fun x -> a := !a @ [x]) [||];
                Expect.toEqual !a []
            );
            test "Barray.forEach" (fun () ->
                let a = ref [] in
                Barray.forEach (fun x -> a := !a @ [x]) [|1; 2; 3|];
                Expect.toEqual !a [1; 2; 3]
            );
            test "Barray.filter empty" (fun () -> 
                [||] |> Barray.filter (fun _ -> true) 
                |> Expect.toEqual [||]
            );
            test "Barray.filter" (fun () ->
                [|1; 2; 3; 4|] |> Barray.filter (fun x -> x mod 2 = 0) 
                |> Expect.toEqual [|2; 4|]
            );
            test "Barray.filterMap empty" (fun () ->
                [||] |> Barray.filterMap (fun x -> Some x)
                |> Expect.toEqual [||]
            );
            test "Barray.filterMap" (fun () -> 
                [|1; 2; 3; 4|] |> Barray.filterMap (fun x -> if x mod 2 = 0 then Some x else Some (x * 2))
                |> Expect.toEqual [|2; 2; 6; 4|]
            );

            test "Barray.flatMap empty" (fun () -> 
                [||] |> Barray.flatMap (fun x -> [|x|])
                |> Expect.toEqual [||]
            );
            test "Barray.flatMap" (fun () ->
                [|1; 2; 3; 4|] |> Barray.flatMap (fun x -> [|1; x|])
                |> Expect.toEqual [|1; 1; 1; 2; 1; 3; 1; 4|]
            );
            test "Barray.range 0" (fun () ->
                let open Barray.Infix in 
                1 -- 2 |> Expect.toEqual [|1; 2|]
            );
            test "Barray.range 1" (fun () ->
                let open Barray.Infix in
                3 -- 1 |> Expect.toEqual [|3; 2; 1|]
            );
            test "Barray.range 2 " (fun () ->
                let open Barray.Infix in
                1 --^ 3 |> Expect.toEqual [|1; 2|]
            );
            test "Barray.range 3" (fun () ->
                let open Barray.Infix in
                3 --^ 1 |> Expect.toEqual [|3; 2|]
            )
        ])