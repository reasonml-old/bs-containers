open Test
open! Bresult

let _ =
  test "make" (fun t ->
      deepEqual t (make 42) (Ok 42));

  test "fail" (fun t ->
      deepEqual t (fail 42) (Error 42));

  test "fromException" (fun t ->
      deepEqual t (fromException Out_of_memory) (Error "Out of memory"));

  test "fromExceptionTrace" (fun t ->
      deepEqual t (fromExceptionTrace Out_of_memory) (Error "Out of memory\n"));

  test "guard - raise" (fun t ->
      deepEqual t (guard (fun () -> raise Out_of_memory)) (Error Out_of_memory));
  test "guard - no raise" (fun t ->
      deepEqual t (guard (fun () -> 42)) (Ok 42));

  test "guardToString - raise" (fun t ->
      deepEqual t (guardToString (fun () -> raise Out_of_memory)) (Error "Out of memory"));
  test "guardToString - no raise" (fun t ->
      deepEqual t (guardToString (fun () -> 42)) (Ok 42));

  test "guardToStringTrace - raise" (fun t ->
      deepEqual t (guardToStringTrace (fun () -> raise Out_of_memory)) (Error "Out of memory\n"));
  test "guardToStringTrace - no raise" (fun t ->
      deepEqual t (guardToStringTrace (fun () -> 42)) (Ok 42));

  test "wrap - raise" (fun t ->
      deepEqual t (wrap (fun _ -> raise Out_of_memory) 21) (Error Out_of_memory));
  test "wrap - no raise" (fun t ->
      deepEqual t (wrap (fun n -> n * 2) 21) (Ok 42));

  test "wrap2 - raise" (fun t ->
      deepEqual t (wrap2 (fun _ -> raise Out_of_memory) 21 2) (Error Out_of_memory));
  test "wrap2 - no raise" (fun t ->
      deepEqual t (wrap2 (fun n m -> n * m) 21 2) (Ok 42));

  test "wrap3 - raise" (fun t ->
      deepEqual t (wrap3 (fun _ -> raise Out_of_memory) 21 2 1) (Error Out_of_memory));
  test "wrap3 - no raise" (fun t ->
      deepEqual t (wrap3 (fun n m o -> n * m - o) 21 2 1) (Ok 41));

  test "isOk - Ok _" (fun t ->
      deepEqual t (isOk (Ok 2)) true);
  test "isOk - Error" (fun t ->
      deepEqual t (isOk (Error "foo")) false);

  test "isError - Ok _" (fun t ->
      deepEqual t (isError (Ok 2)) false);
  test "isError - Error" (fun t ->
      deepEqual t (isError (Error "foo")) true);

  test "equal - =, Ok _ = Ok _" (fun t ->
      deepEqual t (equal (=) (Ok 2) (Ok 2)) true);
  test "equal - <>, Ok _ <> Ok _" (fun t ->
      deepEqual t (equal (<>) (Ok 2) (Ok 2)) false);
  test "equal - <>, Ok _ <> Error _" (fun t ->
      deepEqual t (equal (<>) (Ok 2) (Error "foo")) false);
  test "equal - <>, Error _ <> Ok _" (fun t ->
      deepEqual t (equal (<>) (Error "foo") (Ok 2)) false);
  test "equal - <>, Error _ = Error _" (fun t ->
      deepEqual t (equal (<>) (Error "foo") (Error "foo")) true);

  test "compare - Ok _ = Ok _" (fun t ->
      deepEqual t (compare Comparison.compare (Ok 2) (Ok 2)) Equal);
  test "compare - Ok _ < Ok _" (fun t ->
      deepEqual t (compare Comparison.compare (Ok 2) (Ok 3)) Less);
  test "compare - Ok _ > Error _" (fun t ->
      deepEqual t (compare Comparison.compare (Ok 2) (Error "foo")) Greater);
  test "comapre - Error _ < Ok _" (fun t ->
      deepEqual t (compare Comparison.compare (Error "foo") (Ok 2)) Less);
  test "compare - Error _ = Error _" (fun t ->
      deepEqual t (compare Comparison.compare (Error "foo") (Error "foo")) Equal);

  test "get - Ok _" (fun t ->
      deepEqual t (get "b" (Ok "a")) "a");
  test "get - Error _" (fun t ->
      deepEqual t (get "b" (Error "foo")) "b");

  test "getOr - Ok _" (fun t ->
      deepEqual t (getOr ~default:"b" (Ok "a")) "a");
  test "getOr - Error _" (fun t ->
      deepEqual t (getOr ~default:"b" (Error "foo")) "b");

  test "getOrRaise - Ok _" (fun t ->
      deepEqual t (getOrRaise (Ok "a")) "a");
  test "getOrRaise - Error _" (fun t ->
      try
        ignore @@ getOrRaise (Error "foo");
        Test.fail t
      with
      | _ -> pass t);

  test "getLazy - Ok _" (fun t ->
      deepEqual t (getLazy  (fun () -> "b") (Ok "a")) "a");
  test "getLazy - Error _" (fun t ->
      deepEqual t (getLazy (fun () -> "b") (Error "foo")) "b");

  test "forEach - Ok _" (fun t ->
      plan t 1;
      forEach (fun x -> deepEqual t x "a") (Ok "a"));
  test "forEach - Error _" (fun t ->
      forEach (fun _ -> Test.fail t) (Error "foo"));

  test "map - Ok _" (fun t ->
      deepEqual t (map (( * ) 3) (Ok 7)) (Ok 21));
  test "map - Error _" (fun t ->
      deepEqual t (map (( * ) 3) (Error "foo")) (Error "foo"));

  test "mapOr - Ok _" (fun t ->
      deepEqual t (mapOr ~default:42 (( * ) 3) (Ok 7)) 21);
  test "mapOr - Error _" (fun t ->
      deepEqual t (mapOr ~default:42 (( * ) 3) (Error "foo")) 42);

  test "mapiOrLazy - Ok _" (fun t ->
      deepEqual t (mapOrLazy ~default:(fun () -> 42) (( * ) 3) (Ok 7)) 21);
  test "mapOrLazy - Error _" (fun t ->
      deepEqual t (mapOrLazy ~default:(fun () -> 42) (( * ) 3) (Error "foo")) 42);

  test "maybe - Ok _" (fun t ->
      deepEqual t (maybe (( * ) 3) 42 (Ok 7)) 21);
  test "maybe - Error _" (fun t ->
      deepEqual t (maybe (( * ) 3) 42 (Error "foo")) 42);

  test "map2 - Ok a, Ok b" (fun t ->
      deepEqual t (map2 ( * ) (Ok 7) (Ok 5)) (Ok 35));
  test "map2 - Ok a, Error _" (fun t ->
      deepEqual t (map2 ( * ) (Ok 5) (Error "foo2")) (Error "foo2"));
  test "map2 - Error _, Ok b" (fun t ->
      deepEqual t (map2 ( * ) (Error "foo1") (Ok 5)) (Error "foo1"));
  test "map2 - Error _, Error _" (fun t ->
      deepEqual t (map2 ( * ) (Error "foo1") (Error "foo2")) (Error "foo1"));

  test "mapEither - Ok _" (fun t ->
      deepEqual t (mapEither ((+) 2) ((-) 2) (Ok 5)) (Ok 7));
  test "mapEither - Error _" (fun t ->
      deepEqual t (mapEither ((+) 2) ((-) 2) (Error 5)) (Error (-3)));

  test "catch - Ok _" (fun t ->
      deepEqual t (catch ~ok:((+) 3) ~err:((-) 3) (Ok 7)) 10);
  test "catch - Error _" (fun t ->
      deepEqual t (catch ~ok:((+) 3) ~err:((-) 3) (Error 7)) (-4));

  test "flatMap - Ok _ -> Ok _" (fun t ->
      deepEqual t (flatMap (fun x -> Ok (x * 4)) (Ok 7)) (Ok 28));
  test "flatMap - Ok _ -> Error _" (fun t ->
      deepEqual t (flatMap (fun _ -> (Error "foo")) (Ok 7)) (Error "foo"));
  test "flatMap - Error _" (fun t ->
      deepEqual t (flatMap (fun x -> Ok (x * 4)) (Error "foo")) (Error "foo"));

  test "reduce - Ok _" (fun t ->
      deepEqual t (reduce (+) 6 (Ok 7)) 13);
  test "reduce - Error _" (fun t ->
      deepEqual t (reduce (+) 6 (Error "foo")) 6);

  test "filter - Ok x, f x = true" (fun t ->
      deepEqual t (filter ((=) 3) (Ok 3)) (Ok 3));
  test "filter - Ok x, f x = false" (fun t ->
      deepEqual t (filter (fun _ -> false) (Ok 3)) (Error ()));
  test "filter - Error _" (fun t ->
      deepEqual t (filter (fun _ -> true) (Error "foo")) (Error ()));

  test "apply - Ok f, Ok x" (fun t ->
      deepEqual t (apply (Ok (( * ) 3)) (Ok 7)) (Ok 21));
  test "apply - Ok f, Error _" (fun t ->
      deepEqual t (apply (Ok (( * ) 3)) (Error "foo")) (Error "foo"));
  test "apply - Error _, Ok x" (fun t ->
      deepEqual t (apply (Error "foo") (Ok 7)) (Error "foo"));
  test "apply - Error _, Error _" (fun t ->
      deepEqual t (apply (Error "foo") (Error "foo")) (Error "foo"));

  test "and_ - Ok x, Ok y" (fun t ->
      deepEqual t (and_ (Ok 1) (Ok 2)) (Ok 1));
  test "and_ - Ok x, Error _" (fun t ->
      deepEqual t (and_ (Ok 1) (Error "foo")) (Error "foo"));
  test "and_ - Error _, Ok y" (fun t ->
      deepEqual t (and_ (Error "foo") (Ok 2)) (Error "foo"));
  test "and_ - Error _, Error _" (fun t ->
      deepEqual t (and_ (Error "foo") (Error "foo")) (Error "foo"));

  test "or_ - Ok x, Ok y" (fun t ->
      deepEqual t (or_ ~else_:(Ok 1) (Ok 2)) (Ok 2));
  test "or_ - Ok x, Error _" (fun t ->
      deepEqual t (or_ ~else_:(Ok 1) (Error "foo")) (Ok 1));
  test "or_ - Error _, Ok y" (fun t ->
      deepEqual t (or_ ~else_:(Error "foo") (Ok 2)) (Ok 2));
  test "or_ - Error _, Error _" (fun t ->
      deepEqual t (or_ ~else_:(Error "foo") (Error "foo")) (Error "foo"));

  test "orLazy - Ok x, Ok y" (fun t ->
      deepEqual t (orLazy ~else_:(fun () -> (Ok 1)) (Ok 2)) (Ok 2));
  test "orLazy - Ok x, Error _" (fun t ->
      deepEqual t (orLazy ~else_:(fun () -> (Ok 1)) (Error "foo")) (Ok 1));
  test "orLazy - Error _, Ok y" (fun t ->
      deepEqual t (orLazy ~else_:(fun () -> (Error "foo")) (Ok 2)) (Ok 2));
  test "orLazy - Error _, Error _" (fun t ->
      deepEqual t (orLazy ~else_:(fun () -> (Error "foo")) (Error "foo")) (Error "foo"));

  test "flatten  - Ok (Ok _)" (fun t ->
      deepEqual t (flatten (Ok (Ok 3))) (Ok 3));
  test "flatten - Ok Error _" (fun t ->
      deepEqual t (flatten (Ok (Error "foo"))) (Error "foo"));
  test "flatten - Error _" (fun t ->
      deepEqual t (flatten (Error "foo")) (Error "foo"));

  test "zip - Ok x, Ok y" (fun t ->
      deepEqual t (zip (Ok 4) (Ok 5)) (Ok (4, 5)));
  test "zip - Ok x, Error _" (fun t ->
      deepEqual t (zip (Ok 4) (Error "foo")) (Error "foo"));
  test "zip - Error _, Ok x" (fun t ->
      deepEqual t (zip (Error "foo") (Ok 4)) (Error "foo"));
  test "zip - Error _, Error _" (fun t ->
      deepEqual t (zip (Error "foo") (Error "foo")) (Error "foo"));

  test "any - []" (fun t ->
      deepEqual t (any []) (Error []));
  test "any - [Error _;Error _]" (fun t ->
      deepEqual t (any [(Error "foo");(Error "foo")]) (Error ["foo"; "foo"]));
  test "any - [Error _;Ok 1]" (fun t ->
      deepEqual t (any [(Error "foo");Ok 1]) (Ok 1));
  test "any - [Error _;Ok 1]" (fun t ->
      deepEqual t (any [Ok 1;Ok 2]) (Ok 1));

  test "exists - Ok x, f x = true" (fun t ->
      deepEqual t (exists ((=) 3) (Ok 3)) true);
  test "filter - Ok x, f x = false" (fun t ->
      deepEqual t (exists (fun _ -> false) (Ok 3)) false);
  test "filter - Error _" (fun t ->
      deepEqual t (exists (fun _ -> true) (Error "foo")) false);

  test "forAll - Ok x, f x = true" (fun t ->
      deepEqual t (forAll ((=) 3) (Ok 3)) true);
  test "forAll - Ok x, f x = false" (fun t ->
      deepEqual t (forAll (fun _ -> false) (Ok 3)) false);
  test "forAll - Error _" (fun t ->
      deepEqual t (forAll (fun _ -> true) (Error "foo")) true);

  test "mapList - Ok _" (fun t ->
      deepEqual t (mapList (fun a -> Ok (a + 3)) [1;2;3]) (Ok [4;5;6]));
  test "mapList - Error _" (fun t ->
      deepEqual t (mapList (fun a -> Error (a + 3)) [1;2;3]) (Error 4));

  test "reduceList - Ok _" (fun t ->
      deepEqual t (reduceList (fun acc a -> Ok (acc + a)) 10 [1;2;3]) (Ok 16));
  test "reduceList - Error _" (fun t ->
      deepEqual t (reduceList (fun acc a -> Error (acc + a)) 10 [1;2;3]) (Error 11));

  test "reduceSeq - Ok _" (fun t ->
      let seq f = List.iter f [1;2;3] in
      deepEqual t (reduceSeq (fun acc a -> Ok (acc + a)) 10 seq) (Ok 16));
  test "reduceSeq - Error _" (fun t ->
      let seq f = List.iter f [1;2;3] in
      deepEqual t (reduceSeq (fun acc a -> Error (acc + a)) 10 seq) (Error 11));

  test "retry - Ok _" (fun t ->
      plan t 2; (* 1 pass + 1 deepEqual *)
      deepEqual t (retry 3 (fun () -> ignore @@ pass t; Ok 23)) (Ok 23));
  test "retry - Error _" (fun t ->
      plan t 4; (* 3 pass + 1 deepEqual *)
      deepEqual t (retry 3 (fun () -> ignore @@ pass t; Error "foo")) (Error ["foo"; "foo"; "foo"]));

  test "toOption - Ok _" (fun t ->
      deepEqual t (toOption (Ok "a")) (Some "a"));
  test "toOption - Error _" (fun t ->
      deepEqual t (toOption (Error "foo")) None);

  test "fromOption - Some _" (fun t ->
      deepEqual t (fromOption (Some "a")) (Ok "a"));
  test "fromOption - Error _" (fun t ->
      deepEqual t (fromOption None) (Error ()));

  test "toList - Ok _" (fun t ->
      deepEqual t (toList (Ok "a")) ["a"]);
  test "toList - Error _" (fun t ->
      deepEqual t (toList (Error "foo")) []);

  test "toSeq - Ok _" (fun t ->
      let seq = toSeq (Ok 4) in
      plan t 1;
      seq (function | 4 -> pass t | _ -> Test.fail t));
  test "toSeq - Error _" (fun t ->
      let seq = toSeq (Error "foo") in
      plan t 0;
      seq (function | _ -> Test.fail t));