open InfiniteJest
open InfiniteJest.Test
open Barray

let suite =
    describe "Barray" (fun () -> [
        test "Barray.slice" (fun () -> 
            [||] |> slice ~start:0 ~end_:0
                 |> Expect.toEqual (Some [||])
        );
        test "Barray.slice 1" (fun () ->
            [|1|] |> slice ~start:0 ~end_:1
                  |> Expect.toEqual (Some [|1|])
        );
        test "Barray.slice 2" (fun () ->
            [|1|] |> slice ~start:0 ~end_:0
                  |> Expect.toEqual (Some [||])
        );
        test "Barray.slice 3" (fun () -> 
            [|1; 2; 3|] |> slice ~start:0 ~end_:100
                        |> Expect.toEqual None
        );
        test "Barray.makeFloat" (fun () -> 
            makeFloat 3 |> length
                        |> Expect.toEqual 3
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
        )
    ])