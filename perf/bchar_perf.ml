open Perf

let _ = 
  runPerfTest "getDigit" (fun _ ->
      let c = Bchar.getDigit '1' in
      match c with
      | Some _ -> ()
      | None -> ()
    );
  runPerfTest "getDigitOrRaise" (fun _ -> 
      let _ = (try 
                 Bchar.getDigitOrRaise '1'
               with
               | Failure _ -> 1) in ();
    );
  runPerfTest "getDigitOrRaise Fail" (fun _ ->
      let _ = (try 
                 Bchar.getDigitOrRaise 'a' 
               with 
               | Failure _ -> 1
              ) in (); 
    )
