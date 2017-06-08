open InfiniteJest
open InfiniteJest.Test
open Bopt
open Result

let suite =
  describe "Bopt" (fun () -> [
    test "of_" (fun () ->
      of_ 42
        |> Expect.toEqual (Some 42));

    test "fromList - []" (fun () ->
      [] |> fromList
         |> Expect.toEqual None);
    test "fromList - [\"a\"]" (fun () ->
      ["a"] |> fromList
            |> Expect.toEqual (Some "a"));
    test "fromList - [1;2;3]" (fun () ->
      [1;2;3] |> fromList
              |> Expect.toEqual (Some 1));

    test "if_ - f = true" (fun () ->
      2 |> if_ ((=) 2)
        |> Expect.toEqual (Some 2));
    test "if_ - f = false" (fun () ->
      3 |> if_ ((=) 2)
        |> Expect.toEqual None);

    test "wrap - raise" (fun () ->
      wrap (fun _ -> raise (Invalid_argument "")) 2
        |> Expect.toEqual None);
    test "wrap - raise, handler = true" (fun () ->
      let handler = function | Invalid_argument _ -> true | _ -> false in
      wrap ~handler (fun _ -> raise (Invalid_argument "")) 2
        |> Expect.toEqual None);
    test "wrap - raise, handler = false" (fun () ->
      let handler = function | Invalid_argument _ -> false | _ -> true in
      (fun () ->
        wrap ~handler (fun _ -> raise (Invalid_argument "")) 2)
          |> Expect.toRaise);
    test "wrap - no raise" (fun () ->
      wrap ((+) 3) 2
        |> Expect.toEqual (Some 5));

    test "wrap2 - raise" (fun () ->
      wrap2 (fun _ _ -> raise (Invalid_argument "")) 2 7
        |> Expect.toEqual None);
    test "wrap2 - raise, handler = true" (fun () ->
      let handler = function | Invalid_argument _ -> true | _ -> false in
      wrap2 ~handler (fun _ _ -> raise (Invalid_argument "")) 2 7
        |> Expect.toEqual None);
    test "wrap2 - raise, handler = false" (fun () ->
      let handler = function | Invalid_argument _ -> false | _ -> true in
      (fun () ->
        wrap2 ~handler (fun _ _ -> raise (Invalid_argument "")) 2 7)
          |> Expect.toRaise);
    test "wrap2 - no raise" (fun () ->
      wrap2 (+) 2 7
        |> Expect.toEqual (Some 9));

    test "isSome - Some _" (fun () ->
      Some 2 |> isSome
             |> Expect.toBeTrue);
    test "isSome - None" (fun () ->
      None |> isSome
           |> Expect.toBeFalse);

    test "isNone - Some _" (fun () ->
      Some 2 |> isNone
             |> Expect.toBeFalse);
    test "isNone - None" (fun () ->
      None |> isNone
           |> Expect.toBeTrue);

    test "equal - =, Some _ = Some _" (fun () ->
      equal (=) (Some 2) (Some 2)
        |> Expect.toBeTrue);
    test "equal - <>, Some _ <> Some _" (fun () ->
      equal (<>) (Some 2) (Some 2)
        |> Expect.toBeFalse);
    test "equal - <>, Some _ <> None" (fun () ->
      equal (<>) (Some 2) None
        |> Expect.toBeFalse);
    test "equal - <>, None <> Some _" (fun () ->
      equal (<>) None (Some 2)
        |> Expect.toBeFalse);
    test "equal - <>, None = None" (fun () ->
      equal (<>) None None
        |> Expect.toBeTrue);

    test "compare - Some _ = Some _" (fun () ->
      Bopt.compare Comparator.int (Some 2) (Some 2)
        |> Expect.toEqual Ordering.Equal);
    test "compare - Some _ < Some _" (fun () ->
      Bopt.compare Comparator.int (Some 2) (Some 3)
        |> Expect.toEqual Ordering.Less);
    test "compare - Some _ > None" (fun () ->
      Bopt.compare Comparator.int (Some 2) None
        |> Expect.toEqual Ordering.Greater);
    test "comapre - None < Some _" (fun () ->
      Bopt.compare Comparator.int None (Some 2)
        |> Expect.toEqual Ordering.Less);
    test "compare - None = None" (fun () ->
      Bopt.compare Comparator.int None None
        |> Expect.toEqual Ordering.Equal);

    test "get - Some _" (fun () ->
      Some "a" |> get "b"
               |> Expect.toEqual "a");
    test "get - None" (fun () ->
      None |> get "b"
           |> Expect.toEqual "b");

    test "getOr - Some _" (fun () ->
      Some "a" |> getOr ~default:"b"
               |> Expect.toEqual "a");
    test "getOr - None" (fun () ->
      None |> getOr ~default:"b"
           |> Expect.toEqual "b");

    test "getOrRaise - Some _" (fun () ->
      Some "a" |> getOrRaise
               |> Expect.toEqual "a");
    test "getOrRaise - None" (fun () ->
      (fun () ->
        None |> getOrRaise)
             |> Expect.toRaise);

    test "getLazy - Some _" (fun () ->
      Some "a" |> getLazy  (fun () -> "b")
               |> Expect.toEqual "a");
    test "getLazy - None" (fun () ->
      None |> getLazy (fun () -> "b")
           |> Expect.toEqual "b");

    test "forEach - Some _" (fun () ->
      let xs = ref [] in
      Some "a" |> forEach (fun x -> xs := x :: !xs);
      !xs |> Expect.toEqual ["a"]);
    test "forEach - None" (fun () ->
      let xs = ref [] in
      None |> forEach (fun x -> xs := x :: !xs);
      !xs |> Expect.toEqual []);

    test "find - Some _, true" (fun () ->
      Some 7 |> find (fun x -> x == 7)
             |> Expect.toEqual (Some 7));
    test "find - Some _, false" (fun () ->
      Some 7 |> find (fun x -> x <> 7)
             |> Expect.toEqual (None));
    test "find - None" (fun () ->
      None |> find (fun _ -> true)
           |> Expect.toEqual None);

    test "findOrRaise - Some _, true" (fun () ->
      Some 7 |> findOrRaise (fun x -> x == 7)
             |> Expect.toEqual 7);
    test "findOrRaise - Some _, false" (fun () ->
      (fun  () ->
        Some 7 |> findOrRaise (fun x -> x <> 7))
               |> Expect.toRaise);
    test "findOrRaise - None" (fun () ->
      (fun  () ->
        None |> findOrRaise (fun _ -> true))
             |> Expect.toRaise);

    test "map - Some _" (fun () ->
      Some 7 |> map (( * ) 3)
             |> Expect.toEqual (Some 21));
    test "map - None" (fun () ->
      None |> map (( * ) 3)
           |> Expect.toEqual None);

    test "mapOr - Some _" (fun () ->
      Some 7 |> mapOr ~default:42 (( * ) 3)
             |> Expect.toEqual 21);
    test "mapOr - None" (fun () ->
      None |> mapOr ~default:42 (( * ) 3)
           |> Expect.toEqual 42);

    test "mapiOrLazy - Some _" (fun () ->
      Some 7 |> mapOrLazy ~default:(fun () -> 42) (( * ) 3)
             |> Expect.toEqual 21);
    test "mapOrLazy - None" (fun () ->
      None |> mapOrLazy ~default:(fun () -> 42) (( * ) 3)
           |> Expect.toEqual 42);

    test "maybe - Some _" (fun () ->
      Some 7 |> maybe (( * ) 3) 42
             |> Expect.toEqual 21);
    test "maybe - None" (fun () ->
      None |> maybe (( * ) 3) 42
           |> Expect.toEqual 42);

    test "map2 - Some a, Some b" (fun () ->
      map2 ( * ) (Some 7) (Some 5)
        |> Expect.toEqual (Some 35));
    test "map2 - Some a, None" (fun () ->
      map2 ( * ) (Some 5) None
        |> Expect.toEqual None);
    test "map2 - None, Some b" (fun () ->
      map2 ( * ) None (Some 5)
        |> Expect.toEqual None);
    test "map2 - None, None" (fun () ->
      map2 ( * ) None None
        |> Expect.toEqual None);

    test "flatMap - Some _ -> Some _" (fun () ->
      Some 7 |> flatMap (fun x -> Some (x * 4))
             |> Expect.toEqual (Some 28));
    test "flatMap - Some _ -> None" (fun () ->
      Some 7 |> flatMap (fun _ -> None)
             |> Expect.toEqual None);
    test "flatMap - None" (fun () ->
      None |> flatMap (fun x -> Some (x * 4))
           |> Expect.toEqual None);

    test "reduce - Some _" (fun () ->
      Some 7 |> reduce (+) 6
             |> Expect.toEqual 13);
    test "reduce - None" (fun () ->
      None |> reduce (+) 6
           |> Expect.toEqual 6);

    test "filter - Some x, f x = true" (fun () ->
      Some 3 |> filter ((=) 3)
             |> Expect.toEqual (Some 3));
    test "filter - Some x, f x = false" (fun () ->
      Some 3 |> filter (fun _ -> false)
             |> Expect.toEqual None);
    test "filter - None" (fun () ->
      None |> filter (fun _ -> true)
           |> Expect.toEqual None);

    test "apply - Some f, Some x" (fun () ->
      apply (Some (( * ) 3)) (Some 7)
        |> Expect.toEqual (Some 21));
    test "apply - Some f, None" (fun () ->
      apply (Some (( * ) 3)) None
        |> Expect.toEqual None);
    test "apply - None, Some x" (fun () ->
      apply None (Some 7)
        |> Expect.toEqual None);
    test "apply - None, None" (fun () ->
      apply None None
        |> Expect.toEqual None);

    test "and_ - Some x, Some y" (fun () ->
      Some 2 |> and_ (Some 1)
             |> Expect.toEqual (Some 1));
    test "and_ - Some x, None" (fun () ->
      None |> and_ (Some 1)
           |> Expect.toEqual None);
    test "and_ - None, Some y" (fun () ->
      Some 2 |> and_ None
             |> Expect.toEqual None);
    test "and_ - None, None" (fun () ->
      None |> and_ None
           |> Expect.toEqual None);

    test "or_ - Some x, Some y" (fun () ->
      Some 2 |> or_ ~else_:(Some 1)
             |> Expect.toEqual (Some 2));
    test "or_ - Some x, None" (fun () ->
      None |> or_ ~else_:(Some 1)
           |> Expect.toEqual (Some 1));
    test "or_ - None, Some y" (fun () ->
      Some 2 |> or_ ~else_:None
             |> Expect.toEqual (Some 2));
    test "or_ - None, None" (fun () ->
      None |> or_ ~else_:None
           |> Expect.toEqual None);

    test "orLazy - Some x, Some y" (fun () ->
      Some 2 |> orLazy ~else_:(fun () -> (Some 1))
             |> Expect.toEqual (Some 2));
    test "orLazy - Some x, None" (fun () ->
      None |> orLazy ~else_:(fun () -> (Some 1))
           |> Expect.toEqual (Some 1));
    test "orLazy - None, Some y" (fun () ->
      Some 2 |> orLazy ~else_:(fun () -> None)
             |> Expect.toEqual (Some 2));
    test "orLazy - None, None" (fun () ->
      None |> orLazy ~else_:(fun () -> None)
           |> Expect.toEqual None);

    test "flatten  - Some (Some _)" (fun () ->
      Some (Some 3)
        |> flatten
        |> Expect.toEqual (Some 3));
    test "flatten - Some None" (fun () ->
      Some None
        |> flatten
        |> Expect.toEqual None);
    test "flatten - None" (fun () ->
      None
        |> flatten
        |> Expect.toEqual None);

    test "zip - Some x, Some y" (fun () ->
      zip (Some 4) (Some 5)
        |> Expect.toEqual (Some (4, 5)));
    test "zip - Some x, None" (fun () ->
      zip (Some 4) None
        |> Expect.toEqual None);
    test "zip - None, Some x" (fun () ->
      zip None (Some 4)
        |> Expect.toEqual None);
    test "zip - None, None" (fun () ->
      zip None None
        |> Expect.toEqual None);

    test "any - []" (fun () ->
      [] |> any
         |> Expect.toEqual None);
    test "any - [None;None]" (fun () ->
      [None;None]
        |> any
        |> Expect.toEqual None);
    test "any - [None;Some 1]" (fun () ->
      [None;Some 1]
        |> any
        |> Expect.toEqual (Some 1));
    test "any - [None;Some 1]" (fun () ->
      [Some 1;Some 2]
        |> any
        |> Expect.toEqual (Some 1));

    test "exists - Some x, f x = true" (fun () ->
      Some 3 |> exists ((=) 3)
             |> Expect.toBeTrue);
    test "filter - Some x, f x = false" (fun () ->
      Some 3 |> exists (fun _ -> false)
             |> Expect.toBeFalse);
    test "filter - None" (fun () ->
      None |> exists (fun _ -> true)
           |> Expect.toBeFalse);

    test "forAll - Some x, f x = true" (fun () ->
      Some 3 |> forAll ((=) 3)
             |> Expect.toBeTrue);
    test "forAll - Some x, f x = false" (fun () ->
      Some 3 |> forAll (fun _ -> false)
             |> Expect.toBeFalse);
    test "forAll - None" (fun () ->
      None |> forAll (fun _ -> true)
           |> Expect.toBeTrue);

    test "okOr - Some _" (fun () ->
      Some "a" |> okOr 2
               |> Expect.toEqual (Ok "a"));
    test "okOr - None" (fun () ->
      None |> okOr 2
           |> Expect.toEqual (Error 2));

    test "okOrLazy - Some _" (fun () ->
      Some "a" |> okOrLazy (fun () -> 2)
               |> Expect.toEqual (Ok "a"));
    test "okOrLazy - None" (fun () ->
      None |> okOrLazy (fun () -> 2)
           |> Expect.toEqual (Error 2));

    test "toList - Some _" (fun () ->
      Some "a" |> toList
               |> Expect.toEqual ["a"]);
    test "toList - None" (fun () ->
      None |> toList
           |> Expect.toEqual []);

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