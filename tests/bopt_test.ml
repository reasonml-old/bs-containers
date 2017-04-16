open InfiniteJest
open InfiniteJest.Test
open Bopt
open Result

let suite =
  describe "Bopt" (fun () -> [
    test "make" (fun () ->
      make 42 |> Expect.toEqual (Some 42));

    test "fromList - []" (fun () ->
      fromList [] |> Expect.toEqual None);
    test "fromList - [\"a\"]" (fun () ->
      fromList ["a"] |> Expect.toEqual (Some "a"));
    test "fromList - [1;2;3]" (fun () ->
      fromList [1;2;3] |> Expect.toEqual (Some 1));

    test "if_ - f = true" (fun () ->
      if_ ((=) 2) 2 |> Expect.toEqual (Some 2));
    test "if_ - f = false" (fun () ->
      if_ ((=) 3) 2 |> Expect.toEqual None);

    test "wrap - raise" (fun () ->
      wrap (fun _ -> raise (Invalid_argument "")) 2 |> Expect.toEqual None);
    test "wrap - raise, handler = true" (fun () ->
      let handler = function | Invalid_argument _ -> true | _ -> false in
      wrap ~handler (fun _ -> raise (Invalid_argument "")) 2 |> Expect.toEqual None);
    test "wrap - raise, handler = false" (fun () ->
      let handler = function | Invalid_argument _ -> false | _ -> true in
      (fun () -> wrap ~handler (fun _ -> raise (Invalid_argument "")) 2) |> Expect.toRaise);
    test "wrap - no raise" (fun () ->
      wrap ((+) 3) 2 |> Expect.toEqual (Some 5));

    test "wrap2 - raise" (fun () ->
      wrap2 (fun _ _ -> raise (Invalid_argument "")) 2 7 |> Expect.toEqual None);
    test "wrap2 - raise, handler = true" (fun () ->
      let handler = function | Invalid_argument _ -> true | _ -> false in
      wrap2 ~handler (fun _ _ -> raise (Invalid_argument "")) 2 7 |> Expect.toEqual None);
    test "wrap2 - raise, handler = false" (fun () ->
      let handler = function | Invalid_argument _ -> false | _ -> true in
      (fun () -> wrap2 ~handler (fun _ _ -> raise (Invalid_argument "")) 2 7) |> Expect.toRaise);
    test "wrap2 - no raise" (fun () ->
      wrap2 (+) 2 7 |> Expect.toEqual (Some 9));

    test "isSome - Some _" (fun () ->
      isSome (Some 2) |> Expect.toBeTrue);
    test "isSome - None" (fun () ->
      isSome None |> Expect.toBeFalse);

    test "isNone - Some _" (fun () ->
      isNone (Some 2) |> Expect.toBeFalse);
    test "isNone - None" (fun () ->
      isNone None |> Expect.toBeTrue);

    test "equal - =, Some _ = Some _" (fun () ->
      equal (=) (Some 2) (Some 2) |> Expect.toBeTrue);
    test "equal - <>, Some _ <> Some _" (fun () ->
      equal (<>) (Some 2) (Some 2) |> Expect.toBeFalse);
    test "equal - <>, Some _ <> None" (fun () ->
      equal (<>) (Some 2) None |> Expect.toBeFalse);
    test "equal - <>, None <> Some _" (fun () ->
      equal (<>) None (Some 2) |> Expect.toBeFalse);
    test "equal - <>, None = None" (fun () ->
      equal (<>) None None |> Expect.toBeTrue);

    test "compare - Some _ = Some _" (fun () ->
      Bopt.compare Comparator.int (Some 2) (Some 2) |> Expect.toEqual Ordering.Equal);
    test "compare - Some _ < Some _" (fun () ->
      Bopt.compare Comparator.int (Some 2) (Some 3) |> Expect.toEqual Ordering.Less);
    test "compare - Some _ > None" (fun () ->
      Bopt.compare Comparator.int (Some 2) None |> Expect.toEqual Ordering.Greater);
    test "comapre - None < Some _" (fun () ->
      Bopt.compare Comparator.int None (Some 2) |> Expect.toEqual Ordering.Less);
    test "compare - None = None" (fun () ->
      Bopt.compare Comparator.int None None |> Expect.toEqual Ordering.Equal);

    test "get - Some _" (fun () ->
      get "b" (Some "a") |> Expect.toEqual "a");
    test "get - None" (fun () ->
      get "b" None |> Expect.toEqual "b");

    test "getOr - Some _" (fun () ->
      getOr ~default:"b" (Some "a") |> Expect.toEqual "a");
    test "getOr - None" (fun () ->
      getOr ~default:"b" None |> Expect.toEqual "b");

    test "getOrRaise - Some _" (fun () ->
      getOrRaise (Some "a") |> Expect.toEqual "a");
    test "getOrRaise - None" (fun () ->
      (fun () -> getOrRaise None) |> Expect.toRaise);

    test "getLazy - Some _" (fun () ->
      getLazy  (fun () -> "b") (Some "a") |> Expect.toEqual "a");
    test "getLazy - None" (fun () ->
      getLazy (fun () -> "b") None |> Expect.toEqual "b");

    test "forEach - Some _" (fun () ->
      let xs = ref [] in
      forEach (fun x -> xs := x :: !xs) (Some "a");
      !xs |> Expect.toEqual ["a"]);
    test "forEach - None" (fun () ->
      let xs = ref [] in
      forEach (fun x -> xs := x :: !xs) None;
      !xs |> Expect.toEqual []);

    test "map - Some _" (fun () ->
      map (( * ) 3) (Some 7) |> Expect.toEqual (Some 21));
    test "map - None" (fun () ->
      map (( * ) 3) None |> Expect.toEqual None);

    test "mapOr - Some _" (fun () ->
      mapOr ~default:42 (( * ) 3) (Some 7) |> Expect.toEqual 21);
    test "mapOr - None" (fun () ->
      mapOr ~default:42 (( * ) 3) None |> Expect.toEqual 42);

    test "mapiOrLazy - Some _" (fun () ->
      mapOrLazy ~default:(fun () -> 42) (( * ) 3) (Some 7) |> Expect.toEqual 21);
    test "mapOrLazy - None" (fun () ->
      mapOrLazy ~default:(fun () -> 42) (( * ) 3) None |> Expect.toEqual 42);

    test "maybe - Some _" (fun () ->
      maybe (( * ) 3) 42 (Some 7) |> Expect.toEqual 21);
    test "maybe - None" (fun () ->
      maybe (( * ) 3) 42 None |> Expect.toEqual 42);

    test "map2 - Some a, Some b" (fun () ->
      map2 ( * ) (Some 7) (Some 5) |> Expect.toEqual (Some 35));
    test "map2 - Some a, None" (fun () ->
      map2 ( * ) (Some 5) None |> Expect.toEqual None);
    test "map2 - None, Some b" (fun () ->
      map2 ( * ) None (Some 5) |> Expect.toEqual None);
    test "map2 - None, None" (fun () ->
      map2 ( * ) None None |> Expect.toEqual None);

    test "flatMap - Some _ -> Some _" (fun () ->
      flatMap (fun x -> Some (x * 4)) (Some 7) |> Expect.toEqual (Some 28));
    test "flatMap - Some _ -> None" (fun () ->
      flatMap (fun _ -> None) (Some 7) |> Expect.toEqual None);
    test "flatMap - None" (fun () ->
      flatMap (fun x -> Some (x * 4)) None |> Expect.toEqual None);

    test "reduce - Some _" (fun () ->
      reduce (+) 6 (Some 7) |> Expect.toEqual 13);
    test "reduce - None" (fun () ->
      reduce (+) 6 None |> Expect.toEqual 6);

    test "filter - Some x, f x = true" (fun () ->
      filter ((=) 3) (Some 3) |> Expect.toEqual (Some 3));
    test "filter - Some x, f x = false" (fun () ->
      filter (fun _ -> false) (Some 3) |> Expect.toEqual None);
    test "filter - None" (fun () ->
      filter (fun _ -> true) None |> Expect.toEqual None);

    test "apply - Some f, Some x" (fun () ->
      apply (Some (( * ) 3)) (Some 7) |> Expect.toEqual (Some 21));
    test "apply - Some f, None" (fun () ->
      apply (Some (( * ) 3)) None |> Expect.toEqual None);
    test "apply - None, Some x" (fun () ->
      apply None (Some 7) |> Expect.toEqual None);
    test "apply - None, None" (fun () ->
      apply None None |> Expect.toEqual None);

    test "and_ - Some x, Some y" (fun () ->
      and_ (Some 1) (Some 2) |> Expect.toEqual (Some 1));
    test "and_ - Some x, None" (fun () ->
      and_ (Some 1) None |> Expect.toEqual None);
    test "and_ - None, Some y" (fun () ->
      and_ None (Some 2) |> Expect.toEqual None);
    test "and_ - None, None" (fun () ->
      and_ None None |> Expect.toEqual None);

    test "or_ - Some x, Some y" (fun () ->
      or_ ~else_:(Some 1) (Some 2) |> Expect.toEqual (Some 2));
    test "or_ - Some x, None" (fun () ->
      or_ ~else_:(Some 1) None |> Expect.toEqual (Some 1));
    test "or_ - None, Some y" (fun () ->
      or_ ~else_:None (Some 2) |> Expect.toEqual (Some 2));
    test "or_ - None, None" (fun () ->
      or_ ~else_:None None |> Expect.toEqual None);

    test "orLazy - Some x, Some y" (fun () ->
      orLazy ~else_:(fun () -> (Some 1)) (Some 2) |> Expect.toEqual (Some 2));
    test "orLazy - Some x, None" (fun () ->
      orLazy ~else_:(fun () -> (Some 1)) None |> Expect.toEqual (Some 1));
    test "orLazy - None, Some y" (fun () ->
      orLazy ~else_:(fun () -> None) (Some 2) |> Expect.toEqual (Some 2));
    test "orLazy - None, None" (fun () ->
      orLazy ~else_:(fun () -> None) None |> Expect.toEqual None);

    test "flatten  - Some (Some _)" (fun () ->
      flatten (Some (Some 3)) |> Expect.toEqual (Some 3));
    test "flatten - Some None" (fun () ->
      flatten (Some None) |> Expect.toEqual None);
    test "flatten - None" (fun () ->
      flatten None |> Expect.toEqual None);

    test "zip - Some x, Some y" (fun () ->
      zip (Some 4) (Some 5) |> Expect.toEqual (Some (4, 5)));
    test "zip - Some x, None" (fun () ->
      zip (Some 4) None |> Expect.toEqual None);
    test "zip - None, Some x" (fun () ->
      zip None (Some 4) |> Expect.toEqual None);
    test "zip - None, None" (fun () ->
      zip None None |> Expect.toEqual None);

    test "any - []" (fun () ->
      any [] |> Expect.toEqual None);
    test "any - [None;None]" (fun () ->
      any [None;None] |> Expect.toEqual None);
    test "any - [None;Some 1]" (fun () ->
      any [None;Some 1] |> Expect.toEqual (Some 1));
    test "any - [None;Some 1]" (fun () ->
      any [Some 1;Some 2] |> Expect.toEqual (Some 1));

    test "exists - Some x, f x = true" (fun () ->
      exists ((=) 3) (Some 3) |> Expect.toBeTrue);
    test "filter - Some x, f x = false" (fun () ->
      exists (fun _ -> false) (Some 3) |> Expect.toBeFalse);
    test "filter - None" (fun () ->
      exists (fun _ -> true) None |> Expect.toBeFalse);

    test "forAll - Some x, f x = true" (fun () ->
      forAll ((=) 3) (Some 3) |> Expect.toBeTrue);
    test "forAll - Some x, f x = false" (fun () ->
      forAll (fun _ -> false) (Some 3) |> Expect.toBeFalse);
    test "forAll - None" (fun () ->
      forAll (fun _ -> true) None |> Expect.toBeTrue);

    test "okOr - Some _" (fun () ->
      okOr 2 (Some "a") |> Expect.toEqual (Ok "a"));
    test "okOr - None" (fun () ->
      okOr 2 None |> Expect.toEqual (Error 2));

    test "okOrLazy - Some _" (fun () ->
      okOrLazy (fun () -> 2) (Some "a") |> Expect.toEqual (Ok "a"));
    test "okOrLazy - None" (fun () ->
      okOrLazy (fun () -> 2) None |> Expect.toEqual (Error 2));

    test "toList - Some _" (fun () ->
      toList (Some "a") |> Expect.toEqual ["a"]);
    test "toList - None" (fun () ->
      toList None |> Expect.toEqual []);

    test "toSeq - Some _" (fun () ->
      let xs = ref [] in
      let seq = toSeq (Some 4) in
      seq (fun x -> xs := x :: !xs);
      !xs |> Expect.toEqual [4]);
    test "toSeq - None" (fun () ->
      let xs = ref [] in
      let seq = toSeq None in
      seq (fun x -> xs := x :: !xs);
      !xs |> Expect.toEqual []);
  ])