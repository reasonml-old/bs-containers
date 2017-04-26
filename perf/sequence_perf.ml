open Perf

let it = Barray.toSequence [| 1; 2 ; 3|]

let _ =
    runPerfTest "Barray.toSequence" (fun _ ->
        ignore @@ ([| 1; 2; 3|] |> Barray.toSequence)
    ); 
    runPerfTest "Sequence.map" (fun _ ->
        ignore @@ Sequence.map (fun x -> x * x) it
    );
    runPerfTest "Sequence.foldLeft" (fun _ ->
        Sequence.foldLeft (fun acc x -> x::acc) [] it
    )

