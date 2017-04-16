open InfiniteJest
open InfiniteJest.Test
open Bool

let suite = 
  describe "Bool" (fun () -> [
    test "equals - true, true" (fun () -> 
      equals true true |> Expect.toBeTrue);
    test "equals - true, false" (fun () -> 
      equals true false |> Expect.toBeFalse);
    test "equals - false, true" (fun () -> 
      equals false true |> Expect.toBeFalse);
    test "equals - false, false" (fun () -> 
      equals false false |> Expect.toBeTrue);

    test "compare - true, true" (fun () -> 
      Bool.compare true true |> Expect.toEqual Ordering.Equal);
    test "compare - true, false" (fun () -> 
      Bool.compare true false |> Expect.toEqual Ordering.Greater);
    test "compare - false, true" (fun () -> 
      Bool.compare false true |> Expect.toEqual Ordering.Less);
    test "compare - false, false" (fun () -> 
      Bool.compare false false |> Expect.toEqual Ordering.Equal);
  ])