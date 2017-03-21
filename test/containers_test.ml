

(* Dummy tests for the unit test interface *)
let () = Test.test (fun _ -> true)

let () = Test.test_with_msg "dummy test" (fun x -> Test.truthy x true)