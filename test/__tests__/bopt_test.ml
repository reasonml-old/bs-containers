open Test
open! Bopt

let _ =
  test "make" (fun t ->
      deepEqual t (make 42) (Some 42));

  test "fromList - []" (fun t ->
      deepEqual t (fromList []) None);
  test "fromList - [\"a\"]" (fun t ->
      deepEqual t (fromList ["a"]) (Some "a"));
  test "fromList - [1;2;3]" (fun t ->
      deepEqual t (fromList [1;2;3]) (Some 1));

  test "if_ - f = true" (fun t ->
      deepEqual t (if_ (fun x -> x = 2) 2) (Some 2));
  test "if_ - f = false" (fun t ->
      deepEqual t (if_ (fun x -> x = 3) 2) (None));

  test "wrap - raise" (fun t ->
      deepEqual t
        (wrap (fun _ -> raise (Invalid_argument "")) 2) (None));
  test "wrap - raise, handler = true" (fun t ->
      let handler = function | Invalid_argument _ -> true | _ -> false in
      deepEqual t
        (wrap ~handler (fun _ -> raise (Invalid_argument "")) 2) (None));
  test "wrap - raise, handler = false" (fun t ->
      let handler = function | Invalid_argument _ -> false | _ -> true in
      try 
        ignore @@ wrap ~handler (fun _ -> raise (Invalid_argument "")) 2;
        fail t
      with
      | Invalid_argument _ -> pass t);
  test "wrap - no raise" (fun t ->
      deepEqual t
        (wrap ((+) 3) 2) (Some 5));

  test "wrap2 - raise" (fun t ->
      deepEqual t
        (wrap2 (fun _ _ -> raise (Invalid_argument "")) 2 7) (None));
  test "wrap2 - raise, handler = true" (fun t ->
      let handler = function | Invalid_argument _ -> true | _ -> false in
      deepEqual t
        (wrap2 ~handler (fun _ _ -> raise (Invalid_argument "")) 2 7) (None));
  test "wrap2 - raise, handler = false" (fun t ->
      let handler = function | Invalid_argument _ -> false | _ -> true in
      try 
        ignore @@ wrap2 ~handler (fun _ _ -> raise (Invalid_argument "")) 2 7;
        fail t
      with
      | Invalid_argument _ -> pass t);
  test "wrap2 - no raise" (fun t ->
      deepEqual t
        (wrap2 (+) 2 7) (Some 9));

  test "isSome - Some _" (fun t ->
      deepEqual t (isSome (Some 2)) true);
  test "isSome - None" (fun t ->
      deepEqual t (isSome None) false);

  test "isNone - Some _" (fun t ->
      deepEqual t (isNone (Some 2)) false);
  test "isNone - None" (fun t ->
      deepEqual t (isNone None) true);

  test "equal - =, Some _ = Some _" (fun t ->
      deepEqual t (equal (=) (Some 2) (Some 2)) true);
  test "equal - <>, Some _ <> Some _" (fun t ->
      deepEqual t (equal (<>) (Some 2) (Some 2)) false);
  test "equal - <>, Some _ <> None" (fun t ->
      deepEqual t (equal (<>) (Some 2) None) false);
  test "equal - <>, None <> Some _" (fun t ->
      deepEqual t (equal (<>) None (Some 2)) false);
  test "equal - <>, None = None" (fun t ->
      deepEqual t (equal (<>) None None) true);

  test "compare - Some _ = Some _" (fun t ->
      deepEqual t (compare Comparator.makeDefault (Some 2) (Some 2)) Equal);
  test "compare - Some _ < Some _" (fun t ->
      deepEqual t (compare Comparator.makeDefault (Some 2) (Some 3)) Less);
  test "compare - Some _ > None" (fun t ->
      deepEqual t (compare Comparator.makeDefault (Some 2) None) Greater);
  test "comapre - None < Some _" (fun t ->
      deepEqual t (compare Comparator.makeDefault None (Some 2)) Less);
  test "compare - None = None" (fun t ->
      deepEqual t (compare Comparator.makeDefault None None) Equal);

  test "get - Some _" (fun t ->
      deepEqual t (get "b" (Some "a")) "a");
  test "get - None" (fun t ->
      deepEqual t (get "b" None) "b");

  test "getOr - Some _" (fun t ->
      deepEqual t (getOr ~default:"b" (Some "a")) "a");
  test "getOr - None" (fun t ->
      deepEqual t (getOr ~default:"b" None) "b");

  test "getOrRaise - Some _" (fun t ->
      deepEqual t (getOrRaise (Some "a")) "a");
  test "getOrRaise - None" (fun t ->
      try
        ignore @@ getOrRaise None;
        fail t
      with
      | Invalid_argument _ -> pass t
      | _ -> fail t);

  test "getLazy - Some _" (fun t ->
      deepEqual t (getLazy  (fun () -> "b") (Some "a")) "a");
  test "getLazy - None" (fun t ->
      deepEqual t (getLazy (fun () -> "b") None) "b");

  test "forEach - Some _" (fun t ->
      plan t 1;
      forEach (fun x -> deepEqual t x "a") (Some "a"));
  test "forEach - None" (fun t ->
      forEach (fun _ -> fail t) None);

  test "map - Some _" (fun t ->
      deepEqual t (map (( * ) 3) (Some 7)) (Some 21));
  test "map - None" (fun t ->
      deepEqual t (map (( * ) 3) None) None);

  test "mapOr - Some _" (fun t ->
      deepEqual t (mapOr ~default:42 (( * ) 3) (Some 7)) 21);
  test "mapOr - None" (fun t ->
      deepEqual t (mapOr ~default:42 (( * ) 3) None) 42);

  test "mapiOrLazy - Some _" (fun t ->
      deepEqual t (mapOrLazy ~default:(fun () -> 42) (( * ) 3) (Some 7)) 21);
  test "mapOrLazy - None" (fun t ->
      deepEqual t (mapOrLazy ~default:(fun () -> 42) (( * ) 3) None) 42);

  test "maybe - Some _" (fun t ->
      deepEqual t (maybe (( * ) 3) 42 (Some 7)) 21);
  test "maybe - None" (fun t ->
      deepEqual t (maybe (( * ) 3) 42 None) 42);

  test "map2 - Some a, Some b" (fun t ->
      deepEqual t (map2 ( * ) (Some 7) (Some 5)) (Some 35));
  test "map2 - Some a, None" (fun t ->
      deepEqual t (map2 ( * ) (Some 5) None) None);
  test "map2 - None, Some b" (fun t ->
      deepEqual t (map2 ( * ) None (Some 5)) None);
  test "map2 - None, None" (fun t ->
      deepEqual t (map2 ( * ) None None) None);

  test "flatMap - Some _ -> Some _" (fun t ->
      deepEqual t (flatMap (fun x -> Some (x * 4)) (Some 7)) (Some 28));
  test "flatMap - Some _ -> None" (fun t ->
      deepEqual t (flatMap (fun _ -> None) (Some 7)) None);
  test "flatMap - None" (fun t ->
      deepEqual t (flatMap (fun x -> Some (x * 4)) None) None);

  test "reduce - Some _" (fun t ->
      deepEqual t (reduce (+) 6 (Some 7)) 13);
  test "reduce - None" (fun t ->
      deepEqual t (reduce (+) 6 None) 6);

  test "filter - Some x, f x = true" (fun t ->
      deepEqual t (filter ((=) 3) (Some 3)) (Some 3));
  test "filter - Some x, f x = false" (fun t ->
      deepEqual t (filter (fun _ -> false) (Some 3)) None);
  test "filter - None" (fun t ->
      deepEqual t (filter (fun _ -> true) None) None);

  test "apply - Some f, Some x" (fun t ->
      deepEqual t (apply (Some (( * ) 3)) (Some 7)) (Some 21));
  test "apply - Some f, None" (fun t ->
      deepEqual t (apply (Some (( * ) 3)) None) None);
  test "apply - None, Some x" (fun t ->
      deepEqual t (apply None (Some 7)) None);
  test "apply - None, None" (fun t ->
      deepEqual t (apply None None) None);

  test "and_ - Some x, Some y" (fun t ->
      deepEqual t (and_ (Some 1) (Some 2)) (Some 1));
  test "and_ - Some x, None" (fun t ->
      deepEqual t (and_ (Some 1) None) None);
  test "and_ - None, Some y" (fun t ->
      deepEqual t (and_ None (Some 2)) None);
  test "and_ - None, None" (fun t ->
      deepEqual t (and_ None None) None);

  test "or_ - Some x, Some y" (fun t ->
      deepEqual t (or_ ~else_:(Some 1) (Some 2)) (Some 2));
  test "or_ - Some x, None" (fun t ->
      deepEqual t (or_ ~else_:(Some 1) None) (Some 1));
  test "or_ - None, Some y" (fun t ->
      deepEqual t (or_ ~else_:None (Some 2)) (Some 2));
  test "or_ - None, None" (fun t ->
      deepEqual t (or_ ~else_:None None) None);

  test "orLazy - Some x, Some y" (fun t ->
      deepEqual t (orLazy ~else_:(fun () -> (Some 1)) (Some 2)) (Some 2));
  test "orLazy - Some x, None" (fun t ->
      deepEqual t (orLazy ~else_:(fun () -> (Some 1)) None) (Some 1));
  test "orLazy - None, Some y" (fun t ->
      deepEqual t (orLazy ~else_:(fun () -> None) (Some 2)) (Some 2));
  test "orLazy - None, None" (fun t ->
      deepEqual t (orLazy ~else_:(fun () -> None) None) None);

  test "flatten  - Some (Some _)" (fun t ->
      deepEqual t (flatten (Some (Some 3))) (Some 3));
  test "flatten - Some None" (fun t ->
      deepEqual t (flatten (Some None)) None);
  test "flatten - None" (fun t ->
      deepEqual t (flatten None) None);

  test "zip - Some x, Some y" (fun t ->
      deepEqual t (zip (Some 4) (Some 5)) (Some (4, 5)));
  test "zip - Some x, None" (fun t ->
      deepEqual t (zip (Some 4) None) None);
  test "zip - None, Some x" (fun t ->
      deepEqual t (zip None (Some 4)) None);
  test "zip - None, None" (fun t ->
      deepEqual t (zip None None) None);

  test "any - []" (fun t ->
      deepEqual t (any []) None);
  test "any - [None;None]" (fun t ->
      deepEqual t (any [None;None]) None);
  test "any - [None;Some 1]" (fun t ->
      deepEqual t (any [None;Some 1]) (Some 1));
  test "any - [None;Some 1]" (fun t ->
      deepEqual t (any [Some 1;Some 2]) (Some 1));

  test "exists - Some x, f x = true" (fun t ->
      deepEqual t (exists ((=) 3) (Some 3)) true);
  test "filter - Some x, f x = false" (fun t ->
      deepEqual t (exists (fun _ -> false) (Some 3)) false);
  test "filter - None" (fun t ->
      deepEqual t (exists (fun _ -> true) None) false);

  test "forAll - Some x, f x = true" (fun t ->
      deepEqual t (forAll ((=) 3) (Some 3)) true);
  test "forAll - Some x, f x = false" (fun t ->
      deepEqual t (forAll (fun _ -> false) (Some 3)) false);
  test "forAll - None" (fun t ->
      deepEqual t (forAll (fun _ -> true) None) true);

  test "okOr - Some _" (fun t ->
      deepEqual t (okOr 2 (Some "a")) (Ok "a"));
  test "okOr - None" (fun t ->
      deepEqual t (okOr 2 None) (Error 2));

  test "okOrLazy - Some _" (fun t ->
      deepEqual t (okOrLazy (fun () -> 2) (Some "a")) (Ok "a"));
  test "okOrLazy - None" (fun t ->
      deepEqual t (okOrLazy (fun () -> 2) None) (Error 2));

  test "toList - Some _" (fun t ->
      deepEqual t (toList (Some "a")) ["a"]);
  test "toList - None" (fun t ->
      deepEqual t (toList None) []);

  test "toSeq - Some _" (fun t ->
      let seq = toSeq (Some 4) in
      plan t 1;
      seq (function | 4 -> pass t | _ -> fail t));
  test "toSeq - None" (fun t ->
      let seq = toSeq None in
      plan t 0;
      seq (function | _ -> fail t));