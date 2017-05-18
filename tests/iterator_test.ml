open InfiniteJest
open InfiniteJest.Test

(** 
  Iterator is a abstract type in the sense that we need to have a concrete 
  container to test out its functionality, here we picked Barray
*)

let suite =
  describe "Iterator" (fun () -> [
    test "Barray.toIterator - fromIterator cycle" (fun () ->
      [|1 ; 2; 3|] |> Barray.toIterator
                   |> Barray.fromIterator
                   |> Expect.toEqual [|1; 2; 3|]
        
    );
    test "Iterator.map" (fun () ->
      [|1; 2; 3|] |> Barray.toIterator
                  |> Iterator.map (fun x -> x * 2)
                  |> Barray.fromIterator
                  |> Expect.toEqual [| 2; 4; 6 |]
    );
    test "Iterator.filter" (fun () -> 
      [|1; 2; 3|] |> Barray.toIterator
                  |> Iterator.filter (fun x -> x > 2)
                  |> Barray.fromIterator
                  |> Expect.toEqual [| 3 |]
    );
    test "Iterator.filterMap" (fun () -> 
      [|'1'; '2'; '3'; 'a'|] |> Barray.toIterator
                      |> Iterator.filterMap (fun x -> Bchar.getDigit x)                
                      |> Barray.fromIterator
                      |> Expect.toEqual [|1; 2; 3|]
    )
  ])