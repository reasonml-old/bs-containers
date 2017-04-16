open InfiniteJest
open InfiniteJest.Test
open Bresult

let suite =
  describe "Bresult" (fun () -> [
    test "make" (fun () ->
      make 42 |> Expect.toEqual (Ok 42));

    test "fail" (fun () ->
      fail 42 |> Expect.toEqual (Error 42));

    test "fromException" (fun () ->
      fromException Out_of_memory |> Expect.toEqual (Error "Out of memory"));

    test "fromExceptionTrace" (fun () ->
      Printexc.record_backtrace true;
      fromExceptionTrace Out_of_memory |> Expect.toEqual (Error "Out of memory\n"));

    test "guard - raise" (fun () ->
      guard (fun () -> raise Out_of_memory) |> Expect.toEqual (Error Out_of_memory));
    test "guard - no raise" (fun () ->
      guard (fun () -> 42) |> Expect.toEqual (Ok 42));

    test "guardToString - raise" (fun () ->
      guardToString (fun () -> raise Out_of_memory) |> Expect.toEqual (Error "Out of memory"));
    test "guardToString - no raise" (fun () ->
      guardToString (fun () -> 42) |> Expect.toEqual (Ok 42));

    test "guardToStringTrace - raise" (fun () ->
      Printexc.record_backtrace true;
      guardToStringTrace (fun () -> raise Out_of_memory) |> Expect.toEqual (Error "Out of memory\n"));
    test "guardToStringTrace - no raise" (fun () ->
      guardToStringTrace (fun () -> 42) |> Expect.toEqual (Ok 42));

    test "wrap - raise" (fun () ->
      wrap (fun _ -> raise Out_of_memory) 21 |> Expect.toEqual (Error Out_of_memory));
    test "wrap - no raise" (fun () ->
      wrap (fun n -> n * 2) 21 |> Expect.toEqual (Ok 42));

    test "wrap2 - raise" (fun () ->
      wrap2 (fun _ -> raise Out_of_memory) 21 2 |> Expect.toEqual (Error Out_of_memory));
    test "wrap2 - no raise" (fun () ->
      wrap2 (fun n m -> n * m) 21 2 |> Expect.toEqual (Ok 42));

    test "wrap3 - raise" (fun () ->
      wrap3 (fun _ -> raise Out_of_memory) 21 2 1 |> Expect.toEqual (Error Out_of_memory));
    test "wrap3 - no raise" (fun () ->
      wrap3 (fun n m o -> n * m - o) 21 2 1 |> Expect.toEqual (Ok 41));

    test "isOk - Ok _" (fun () ->
      isOk (Ok 2) |> Expect.toBeTrue);
    test "isOk - Error" (fun () ->
      isOk (Error "foo") |> Expect.toBeFalse);

    test "isError - Ok _" (fun () ->
      isError (Ok 2) |> Expect.toBeFalse);
    test "isError - Error" (fun () ->
      isError (Error "foo") |> Expect.toBeTrue);

    test "equal - =, Ok _ = Ok _" (fun () ->
      equal (=) (Ok 2) (Ok 2) |> Expect.toBeTrue);
    test "equal - <>, Ok _ <> Ok _" (fun () ->
      equal (<>) (Ok 2) (Ok 2) |> Expect.toBeFalse);
    test "equal - <>, Ok _ <> Error _" (fun () ->
      equal (<>) (Ok 2) (Error "foo") |> Expect.toBeFalse);
    test "equal - <>, Error _ <> Ok _" (fun () ->
      equal (<>) (Error "foo") (Ok 2) |> Expect.toBeFalse);
    test "equal - <>, Error _ = Error _" (fun () ->
      equal (<>) (Error "foo") (Error "foo") |> Expect.toBeTrue);

    test "compare - Ok _ = Ok _" (fun () ->
      Bresult.compare Comparator.int (Ok 2) (Ok 2) |> Expect.toEqual Ordering.Equal);
    test "compare - Ok _ < Ok _" (fun () ->
      Bresult.compare Comparator.int (Ok 2) (Ok 3) |> Expect.toEqual Ordering.Less);
    test "compare - Ok _ > Error _" (fun () ->
      Bresult.compare Comparator.int (Ok 2) (Error "foo") |> Expect.toEqual Ordering.Greater);
    test "comapre - Error _ < Ok _" (fun () ->
      Bresult.compare Comparator.int (Error "foo") (Ok 2) |> Expect.toEqual Ordering.Less);
    test "compare - Error _ = Error _" (fun () ->
      Bresult.compare Comparator.int (Error "foo") (Error "foo") |> Expect.toEqual Ordering.Equal);

    test "get - Ok _" (fun () ->
      get "b" (Ok "a") |> Expect.toEqual "a");
    test "get - Error _" (fun () ->
      get "b" (Error "foo") |> Expect.toEqual "b");

    test "getOr - Ok _" (fun () ->
      getOr ~default:"b" (Ok "a") |> Expect.toEqual "a");
    test "getOr - Error _" (fun () ->
      getOr ~default:"b" (Error "foo") |> Expect.toEqual "b");

    test "getOrRaise - Ok _" (fun () ->
      getOrRaise (Ok "a") |> Expect.toEqual "a");
    test "getOrRaise - Error _" (fun () ->
      (fun () -> getOrRaise (Error "foo")) |> Expect.toRaise);

    test "getLazy - Ok _" (fun () ->
      getLazy  (fun () -> "b") (Ok "a") |> Expect.toEqual "a");
    test "getLazy - Error _" (fun () ->
      getLazy (fun () -> "b") (Error "foo") |> Expect.toEqual "b");

    test "forEach - Ok _" (fun () ->
      let xs = ref [] in
      forEach (fun x -> xs := x :: !xs) (Ok "a");
      !xs |> Expect.toEqual ["a"]);
    test "forEach - Error _" (fun () ->
      let xs = ref [] in
      forEach (fun x -> xs := x :: !xs) (Error "foo");
      !xs |> Expect.toEqual []);

    test "map - Ok _" (fun () ->
      map (( * ) 3) (Ok 7) |> Expect.toEqual (Ok 21));
    test "map - Error _" (fun () ->
      map (( * ) 3) (Error "foo") |> Expect.toEqual (Error "foo"));

    test "mapOr - Ok _" (fun () ->
      mapOr ~default:42 (( * ) 3) (Ok 7) |> Expect.toEqual 21);
    test "mapOr - Error _" (fun () ->
      mapOr ~default:42 (( * ) 3) (Error "foo") |> Expect.toEqual 42);

    test "mapiOrLazy - Ok _" (fun () ->
      mapOrLazy ~default:(fun () -> 42) (( * ) 3) (Ok 7) |> Expect.toEqual 21);
    test "mapOrLazy - Error _" (fun () ->
      mapOrLazy ~default:(fun () -> 42) (( * ) 3) (Error "foo") |> Expect.toEqual 42);

    test "maybe - Ok _" (fun () ->
      maybe (( * ) 3) 42 (Ok 7) |> Expect.toEqual 21);
    test "maybe - Error _" (fun () ->
      maybe (( * ) 3) 42 (Error "foo") |> Expect.toEqual 42);

    test "map2 - Ok a, Ok b" (fun () ->
      map2 ( * ) (Ok 7) (Ok 5) |> Expect.toEqual (Ok 35));
    test "map2 - Ok a, Error _" (fun () ->
      map2 ( * ) (Ok 5) (Error "foo2") |> Expect.toEqual (Error "foo2"));
    test "map2 - Error _, Ok b" (fun () ->
      map2 ( * ) (Error "foo1") (Ok 5) |> Expect.toEqual (Error "foo1"));
    test "map2 - Error _, Error _" (fun () ->
      map2 ( * ) (Error "foo1") (Error "foo2") |> Expect.toEqual (Error "foo1"));

    test "mapEither - Ok _" (fun () ->
      mapEither ((+) 2) ((-) 2) (Ok 5) |> Expect.toEqual (Ok 7));
    test "mapEither - Error _" (fun () ->
      mapEither ((+) 2) ((-) 2) (Error 5) |> Expect.toEqual (Error (-3)));

    test "catch - Ok _" (fun () ->
      catch ~ok:((+) 3) ~err:((-) 3) (Ok 7) |> Expect.toEqual 10);
    test "catch - Error _" (fun () ->
      catch ~ok:((+) 3) ~err:((-) 3) (Error 7) |> Expect.toEqual (-4));

    test "flatMap - Ok _ -> Ok _" (fun () ->
      flatMap (fun x -> Ok (x * 4)) (Ok 7) |> Expect.toEqual (Ok 28));
    test "flatMap - Ok _ -> Error _" (fun () ->
      flatMap (fun _ -> (Error "foo")) (Ok 7) |> Expect.toEqual (Error "foo"));
    test "flatMap - Error _" (fun () ->
      flatMap (fun x -> Ok (x * 4)) (Error "foo") |> Expect.toEqual (Error "foo"));

    test "reduce - Ok _" (fun () ->
      reduce (+) 6 (Ok 7) |> Expect.toEqual 13);
    test "reduce - Error _" (fun () ->
      reduce (+) 6 (Error "foo") |> Expect.toEqual 6);

    test "filter - Ok x, f x = true" (fun () ->
      filter ((=) 3) (Ok 3) |> Expect.toEqual (Ok 3));
    test "filter - Ok x, f x = false" (fun () ->
      filter (fun _ -> false) (Ok 3) |> Expect.toEqual (Error ()));
    test "filter - Error _" (fun () ->
      filter (fun _ -> true) (Error "foo") |> Expect.toEqual (Error ()));

    test "apply - Ok f, Ok x" (fun () ->
      apply (Ok (( * ) 3)) (Ok 7) |> Expect.toEqual (Ok 21));
    test "apply - Ok f, Error _" (fun () ->
      apply (Ok (( * ) 3)) (Error "foo") |> Expect.toEqual (Error "foo"));
    test "apply - Error _, Ok x" (fun () ->
      apply (Error "foo") (Ok 7) |> Expect.toEqual (Error "foo"));
    test "apply - Error _, Error _" (fun () ->
      apply (Error "foo") (Error "foo") |> Expect.toEqual (Error "foo"));

    test "and_ - Ok x, Ok y" (fun () ->
      and_ (Ok 1) (Ok 2) |> Expect.toEqual (Ok 1));
    test "and_ - Ok x, Error _" (fun () ->
      and_ (Ok 1) (Error "foo") |> Expect.toEqual (Error "foo"));
    test "and_ - Error _, Ok y" (fun () ->
      and_ (Error "foo") (Ok 2) |> Expect.toEqual (Error "foo"));
    test "and_ - Error _, Error _" (fun () ->
      and_ (Error "foo") (Error "foo") |> Expect.toEqual (Error "foo"));

    test "or_ - Ok x, Ok y" (fun () ->
      or_ ~else_:(Ok 1) (Ok 2) |> Expect.toEqual (Ok 2));
    test "or_ - Ok x, Error _" (fun () ->
      or_ ~else_:(Ok 1) (Error "foo") |> Expect.toEqual (Ok 1));
    test "or_ - Error _, Ok y" (fun () ->
      or_ ~else_:(Error "foo") (Ok 2) |> Expect.toEqual (Ok 2));
    test "or_ - Error _, Error _" (fun () ->
      or_ ~else_:(Error "foo") (Error "foo") |> Expect.toEqual (Error "foo"));

    test "orLazy - Ok x, Ok y" (fun () ->
      orLazy ~else_:(fun () -> (Ok 1)) (Ok 2) |> Expect.toEqual (Ok 2));
    test "orLazy - Ok x, Error _" (fun () ->
      orLazy ~else_:(fun () -> (Ok 1)) (Error "foo") |> Expect.toEqual (Ok 1));
    test "orLazy - Error _, Ok y" (fun () ->
      orLazy ~else_:(fun () -> (Error "foo")) (Ok 2) |> Expect.toEqual (Ok 2));
    test "orLazy - Error _, Error _" (fun () ->
      orLazy ~else_:(fun () -> (Error "foo")) (Error "foo") |> Expect.toEqual (Error "foo"));

    test "flatten  - Ok (Ok _)" (fun () ->
      flatten (Ok (Ok 3)) |> Expect.toEqual (Ok 3));
    test "flatten - Ok Error _" (fun () ->
      flatten (Ok (Error "foo")) |> Expect.toEqual (Error "foo"));
    test "flatten - Error _" (fun () ->
      flatten (Error "foo") |> Expect.toEqual (Error "foo"));

    test "zip - Ok x, Ok y" (fun () ->
      zip (Ok 4) (Ok 5) |> Expect.toEqual (Ok (4, 5)));
    test "zip - Ok x, Error _" (fun () ->
      zip (Ok 4) (Error "foo") |> Expect.toEqual (Error "foo"));
    test "zip - Error _, Ok x" (fun () ->
      zip (Error "foo") (Ok 4) |> Expect.toEqual (Error "foo"));
    test "zip - Error _, Error _" (fun () ->
      zip (Error "foo") (Error "foo") |> Expect.toEqual (Error "foo"));

    test "any - []" (fun () ->
      any [] |> Expect.toEqual (Error []));
    test "any - [Error _;Error _]" (fun () ->
      any [(Error "foo");(Error "foo")] |> Expect.toEqual (Error ["foo"; "foo"]));
    test "any - [Error _;Ok 1]" (fun () ->
      any [(Error "foo");Ok 1] |> Expect.toEqual (Ok 1));
    test "any - [Error _;Ok 1]" (fun () ->
      any [Ok 1;Ok 2] |> Expect.toEqual (Ok 1));

    test "exists - Ok x, f x = true" (fun () ->
      exists ((=) 3) (Ok 3) |> Expect.toBeTrue);
    test "filter - Ok x, f x = false" (fun () ->
      exists (fun _ -> false) (Ok 3) |> Expect.toBeFalse);
    test "filter - Error _" (fun () ->
      exists (fun _ -> true) (Error "foo") |> Expect.toBeFalse);

    test "forAll - Ok x, f x = true" (fun () ->
      forAll ((=) 3) (Ok 3) |> Expect.toBeTrue);
    test "forAll - Ok x, f x = false" (fun () ->
      forAll (fun _ -> false) (Ok 3) |> Expect.toBeFalse);
    test "forAll - Error _" (fun () ->
      forAll (fun _ -> true) (Error "foo") |> Expect.toBeTrue);

    test "mapList - Ok _" (fun () ->
      mapList (fun a -> Ok (a + 3)) [1;2;3] |> Expect.toEqual (Ok [4;5;6]));
    test "mapList - Error _" (fun () ->
      mapList (fun a -> Error (a + 3)) [1;2;3] |> Expect.toEqual (Error 4));

    test "reduceList - Ok _" (fun () ->
      reduceList (fun acc a -> Ok (acc + a)) 10 [1;2;3] |> Expect.toEqual (Ok 16));
    test "reduceList - Error _" (fun () ->
      reduceList (fun acc a -> Error (acc + a)) 10 [1;2;3] |> Expect.toEqual (Error 11));

    test "reduceSeq - Ok _" (fun () ->
      let seq f = List.iter f [1;2;3] in
      reduceSeq (fun acc a -> Ok (acc + a)) 10 seq |> Expect.toEqual (Ok 16));
    test "reduceSeq - Error _" (fun () ->
      let seq f = List.iter f [1;2;3] in
      reduceSeq (fun acc a -> Error (acc + a)) 10 seq |> Expect.toEqual (Error 11));

    test "retry - Ok _" (fun () ->
      let attempts = ref 0 in
      (retry 3 (fun () -> attempts := !attempts + 1; Ok 23), !attempts)
      |> Expect.toEqual (Ok 23, 1));
    test "retry - Error _" (fun () ->
      let attempts = ref 0 in
      (retry 3 (fun () -> attempts := !attempts + 1; Error "foo"), !attempts)
      |> Expect.toEqual (Error ["foo"; "foo"; "foo"], 3));

    test "toOption - Ok _" (fun () ->
      toOption (Ok "a") |> Expect.toEqual (Some "a"));
    test "toOption - Error _" (fun () ->
      toOption (Error "foo") |> Expect.toEqual None);

    test "fromOption - Some _" (fun () ->
      fromOption (Some "a") |> Expect.toEqual (Ok "a"));
    test "fromOption - Error _" (fun () ->
      fromOption None |> Expect.toEqual (Error ()));

    test "toList - Ok _" (fun () ->
      toList (Ok "a") |> Expect.toEqual ["a"]);
    test "toList - Error _" (fun () ->
      toList (Error "foo") |> Expect.toEqual []);

    test "toSeq - Ok _" (fun () ->
      let xs = ref [] in
      let seq = toSeq (Ok 4) in
      seq (fun x -> xs := x :: !xs);
      !xs |> Expect.toEqual [4]);
    test "toSeq - Error _" (fun () ->
      let xs = ref [] in
      let seq = toSeq (Error "foo") in
      seq (fun x -> xs := x :: !xs);
      !xs |> Expect.toEqual []);
  ])