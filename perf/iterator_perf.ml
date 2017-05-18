open Perf

let _ = 
    runPerfTest "Iterator.map" (fun _ ->
        [| 1; 2; 3|] |> Barray.toIterator
                     |> Iterator.map (fun x -> x * x) 
    );

