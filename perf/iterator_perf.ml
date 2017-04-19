open Perf

let it = Barray.toIterator [| 1; 2 ; 3|]

let _ =
    runPerfTest "Barray.toIterator" (fun _ ->
        ignore @@ ([| 1; 2; 3|] |> Barray.toIterator)
    ); 
    runPerfTest "Iterator.map" (fun _ ->
        ignore @@ Iterator.map (fun x -> x * x) it
    );
    runPerfTest "Iterator.foldLeft" (fun _ ->
        Iterator.foldLeft (fun acc x -> x::acc) [] it
    )

