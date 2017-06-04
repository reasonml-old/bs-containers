open Perf

let it = Barray.toSequence [| 1; 2 ; 3|]

let _ =
    runPerfTest "Barray.toSequence" (fun _ ->
        ignore @@ ([| 1; 2; 3|] |> Barray.toSequence)
    ); 
    runPerfTest "Sequence.map" (fun _ ->
        ignore @@ Bsequence.map (fun x -> x * x) it
    );
    runPerfTest "Sequence.foldLeft" (fun _ ->
        Bsequence.reduce (fun acc x -> x::acc) [] it
    )

