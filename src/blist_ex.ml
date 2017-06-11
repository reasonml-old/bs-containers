open Blist

type 'a t = 'a list

let fastSort = List.fast_sort
let stableSort = List.stable_sort
let exists2 = List.exists2
let forAll2 = List.for_all2
let reduceRight2 = List.fold_right2
let reduceLeft2 = List.fold_left2
let reverseAndMap = List.rev_map
let reverseAndMap2 = List.rev_map2
let map2 = List.map2
let forEach2 = List.iter2
let includesByReference = List.memq


let consMaybe o l = match o with
  | Some x -> x :: l
  | None -> l

(*$T
  cons_maybe (Some 1) [2;3] = [1;2;3]
  cons_maybe None [2;3] = [2;3]
*)

let reduceAndMap f acc l =
  let rec aux f acc map_acc l = match l with
    | [] -> acc, reverse map_acc
    | x :: l' ->
      let acc, y = f acc x in
      aux f acc (y :: map_acc) l'
  in
  aux f acc [] l

(*$=
  (6, ["1"; "2"; "3"]) \
    (fold_map (fun acc x->acc+x, string_of_int x) 0 [1;2;3])
*)

(*$Q
  Q.(list int) (fun l -> \
    fold_map (fun acc x -> x::acc, x) [] l = (List.rev l, l))
*)

let reduceAndMap2 f acc l1 l2 =
  let rec aux f acc map_acc l1 l2 = match l1, l2 with
    | [], [] -> acc, reverse map_acc
    | [], _
    | _, [] -> invalid_arg "fold_map2"
    | x1 :: l1', x2 :: l2' ->
      let acc, y = f acc x1 x2 in
      aux f acc (y :: map_acc) l1' l2'
  in
  aux f acc [] l1 l2

(*$=
  (310, ["1 10"; "2 0"; "3 100"]) \
    (fold_map2 (fun acc x y->acc+x*y, string_of_int x ^ " " ^ string_of_int y) \
    0 [1;2;3] [10;0;100])
*)

(*$T
  (try ignore (fold_map2 (fun _ _ _ -> assert false) 42 [] [1]); false \
   with Invalid_argument _ -> true)
*)

let reduceAndFilterAndMap f acc l =
  let rec aux f acc map_acc l = match l with
    | [] -> acc, reverse map_acc
    | x :: l' ->
      let acc, y = f acc x in
      aux f acc (consMaybe y map_acc) l'
  in
  aux f acc [] l

(*$= & ~printer:Q.Print.(pair int (list int))
  (List.fold_left (+) 0 (1--10), [2;4;6;8;10]) \
  (fold_filter_map (fun acc x -> acc+x, if x mod 2 = 0 then Some x else None) \
    0 (1--10))
*)

let reduceAndFlatmap f acc l =
  let rec aux f acc map_acc l = match l with
    | [] -> acc, reverse map_acc
    | x :: l' ->
      let acc, y = f acc x in
      aux f acc (reverseAndAppend y map_acc) l'
  in
  aux f acc [] l

(*$=
  (6, ["1"; "a1"; "2"; "a2"; "3"; "a3"]) \
    (let pf = Printf.sprintf in \
      fold_flat_map (fun acc x->acc+x, [pf "%d" x; pf "a%d" x]) 0 [1;2;3])
*)

(*$Q
  Q.(list int) (fun l -> \
    fold_flat_map (fun acc x -> x::acc, [x;x+10]) [] l = \
      (List.rev l, flat_map (fun x->[x;x+10]) l) )
*)

let product f l1 l2 =
  flatMap (fun x -> Blist.map (fun y -> f x y) l2) l1

let reduceProduct f acc l1 l2 =
  reduceLeft
    (fun acc x1 ->
       reduceLeft
         (fun acc x2 -> f acc x1 x2)
         acc l2
    ) acc l1

let diagonal l =
  let rec gen acc l = match l with
    | [] -> acc
    | x::l' ->
      let acc = reduceLeft (fun acc y -> (x,y) :: acc) acc l' in
      gen acc l'
  in
  gen [] l

(*$T
  diagonal [] = []
  diagonal [1] = []
  diagonal [1;2] = [1,2]
  diagonal [1;2;3] |> List.sort Pervasives.compare = [1, 2; 1, 3; 2, 3]
*)

let partitionMap f l =
  let rec forEach f l1 l2 l = match l with
    | [] -> reverse l1, reverse l2
    | x :: tl ->
      match f x with
      | `Left y -> forEach f (y :: l1) l2 tl
      | `Right y -> forEach f l1 (y :: l2) tl
      | `Drop -> forEach f l1 l2 tl
  in
  forEach f [] [] l

(*$R
  let l1, l2 =
    partition_map (function
      | n when n = 0 -> `Drop
      | n when n mod 2 = 0 -> `Left n
      | n -> `Right n
    ) [0;1;2;3;4]
  in
  assert_equal [2;4] l1;
  assert_equal [1;3] l2
*)

let sortedMerge ?(cmp=Pervasives.compare) l1 l2 =
  let rec recurse cmp acc l1 l2 = match l1,l2 with
    | [], _ -> reverseAndAppend acc l2
    | _, [] -> reverseAndAppend acc l1
    | x1::l1', x2::l2' ->
      let c = cmp x1 x2 in
      if c < 0 then recurse cmp (x1::acc) l1' l2
      else if c > 0 then recurse cmp (x2::acc) l1 l2'
      else recurse cmp (x1::x2::acc) l1' l2'
  in
  recurse cmp [] l1 l2

(*$T
  List.sort Pervasives.compare ([(( * )2); ((+)1)] <*> [10;100]) \
    = [11; 20; 101; 200]
  sorted_merge [1;1;2] [1;2;3] = [1;1;1;2;2;3]
*)

(*$Q
  Q.(pair (list int) (list int)) (fun (l1,l2) -> \
    List.length (sorted_merge l1 l2) = List.length l1 + List.length l2)
*)

let sortUnique (type elt) ?(cmp=Pervasives.compare) l =
  let module S = Set.Make(struct
      type t = elt
      let compare = cmp
    end) in
  let set = reduceRight S.add l S.empty in
  S.elements set

(*$T
  sort_uniq [1;2;5;3;6;1;4;2;3] = [1;2;3;4;5;6]
  sort_uniq [] = []
  sort_uniq [10;10;10;10;1;10] = [1;10]
*)

let isSorted ?(cmp=Pervasives.compare) l =
  let rec aux cmp = function
    | [] | [_] -> true
    | x :: ((y :: _) as tail) -> cmp x y <= 0 && aux cmp tail
  in
  aux cmp l

(*$Q
  Q.(list small_int) (fun l -> \
    is_sorted (List.sort Pervasives.compare l))
*)

let sortedInsert ?(cmp=Pervasives.compare) ?(uniq=false) x l =
  let rec aux cmp uniq x left l = match l with
    | [] -> reverseAndAppend left [x]
    | y :: tail ->
      match cmp x y with
      | 0 ->
        let l' = if uniq then l else x :: l in
        reverseAndAppend left l'
      | n when n<0 -> reverseAndAppend left (x :: l)
      | _ -> aux cmp uniq x (y::left) tail
  in
  aux cmp uniq x [] l

(*$Q
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      is_sorted (sorted_insert ~uniq:true x l))
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      is_sorted (sorted_insert ~uniq:false x l))
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      let l' = sorted_insert ~uniq:false x l in \
      List.length l' = List.length l + 1)
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      List.mem x (sorted_insert x l))
*)

let uniqueSuccessors ?(eq=(=)) l =
  let rec f acc l = match l with
    | [] -> reverse acc
    | [x] -> reverse (x::acc)
    | x :: ((y :: _) as tail) when eq x y -> f acc tail
    | x :: tail -> f (x::acc) tail
  in
  f [] l

(*$T
  uniq_succ [1;1;2;3;1;6;6;4;6;1] = [1;2;3;1;6;4;6;1]
*)

let groupDuplicateSuccessors ?(eq=(=)) l =
  let rec f ~eq acc cur l = match cur, l with
    | [], [] -> reverse acc
    | _::_, [] -> reverse (reverse cur :: acc)
    | [], x::tl -> f ~eq acc [x] tl
    | (y :: _), x :: tl when eq x y -> f ~eq acc (x::cur) tl
    | _, x :: tl -> f ~eq (reverse cur :: acc) [x] tl
  in
  f ~eq [] [] l

(*$T
  group_succ [1;2;3;1;1;2;4] = [[1]; [2]; [3]; [1;1]; [2]; [4]]
  group_succ [] = []
  group_succ [1;1;1] = [[1;1;1]]
  group_succ [1;2;2;2] = [[1]; [2;2;2]]
  group_succ ~eq:(fun (x,_)(y,_)-> x=y) [1, 1; 1, 2; 1, 3; 2, 0] \
    = [[1, 1; 1, 2; 1, 3]; [2, 0]]
*)

let sortedMergeUnique ?(cmp=Pervasives.compare) l1 l2 =
  let push ~cmp acc x = match acc with
    | [] -> [x]
    | y :: _ when cmp x y > 0 -> x :: acc
    | _ -> acc (* duplicate, do not yield *)
  in
  let rec recurse ~cmp acc l1 l2 = match l1,l2 with
    | [], l
    | l, [] ->
      let acc = reduceLeft (push ~cmp) acc l in
      reverse acc
    | x1::l1', x2::l2' ->
      let c = cmp x1 x2 in
      if c < 0 then recurse ~cmp (push ~cmp acc x1) l1' l2
      else if c > 0 then recurse ~cmp (push ~cmp acc x2) l1 l2'
      else recurse ~cmp acc l1 l2' (* drop one of the [x] *)
  in
  recurse ~cmp [] l1 l2

(*$T
  sorted_merge_uniq [1; 1; 2; 3; 5; 8] [1; 2; 3; 4; 6; 8; 9; 9] = [1;2;3;4;5;6;8;9]
*)

(*$Q
  Q.(list int) (fun l -> \
    let l = List.sort Pervasives.compare l in \
    sorted_merge_uniq l [] = uniq_succ l)
  Q.(list int) (fun l -> \
    let l = List.sort Pervasives.compare l in \
    sorted_merge_uniq [] l = uniq_succ l)
  Q.(pair (list int) (list int)) (fun (l1, l2) -> \
    let l1 = List.sort Pervasives.compare l1 \
    and l2 = List.sort Pervasives.compare l2 in \
    let l3 = sorted_merge_uniq l1 l2 in \
    uniq_succ l3 = l3)
*)

let headAndTail = function
  | [] -> failwith "hd_tl"
  | x :: l -> x, l

(*$T
  try ignore (hd_tl []); false with Failure _ -> true
  hd_tl [1;2;3] = (1, [2;3])
*)

let sublistsOfLength ?(last=fun _ -> None) ?offset n l =
  if n < 1 then invalid_arg "sublists_of_len: n must be > 0";
  let offset = match offset with
    | None -> n
    | Some o when o < 1 -> invalid_arg "sublists_of_len: offset must be > 0"
    | Some o -> o
  in
  (* add sub-lists of [l] to [acc] *)
  let rec aux acc l =
    let group = take n l in
    if group=[] then acc (* this was the last group, we are done *)
    else if length group < n (* last group, with missing elements *)
    then match last group with
      | None -> acc
      | Some group' -> group' :: acc
    else (
      let l' = skip offset l in
      aux (group :: acc) l' (* continue *)
    )
  in
  reverse (aux [] l)

(*$= sublists_of_len as subs & ~printer:Q.Print.(list (list int))
  [[1;2;3]] (subs 3 [1;2;3;4])
  [[1;2]; [3;4]; [5;6]] (subs 2 [1;2;3;4;5;6])
  [] (subs 3 [1;2])
  [[1;2];[3;4]] (subs 2 ~offset:2 [1;2;3;4])
  [[1;2];[2;3]] (subs 2 ~offset:1 [1;2;3])
  [[1;2];[4;5]] (subs 2 ~offset:3 [1;2;3;4;5;6])
  [[1;2;3];[4]] (subs 3 ~last:CCOpt.return [1;2;3;4])
  [[1;2]; [3;4]] (subs 2 [1;2;3;4;5])
*)

(*$T
  find_map (fun x -> if x=3 then Some "a" else None) [1;2;3;4] = Some "a"
  find_map (fun x -> if x=3 then Some "a" else None) [1;2;4;5] = None
*)

let filterMap f l =
  let rec recurse acc l = match l with
    | [] -> reverse acc
    | x::l' ->
      let acc' = match f x with | None -> acc | Some y -> y::acc in
      recurse acc' l'
  in recurse [] l

(*$=
  ["2"; "4"] \
    (filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
      [1;2;3;4;5])
  [ "2"; "4"; "6" ] \
    (filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
      [ 1; 2; 3; 4; 5; 6 ])
*)

let findAndMapi f l =
  let rec aux f i = function
    | [] -> None
    | x::l' ->
      match f i x with
      | Some _ as res -> res
      | None -> aux f (i+1) l'
  in aux f 0 l

let findAndMap f l = findAndMapi (fun _ -> f) l

let findWithIndex p l = findAndMapi (fun i x -> if p x then Some (i, x) else None) l

(*$T
  find_map (fun x -> if x=3 then Some "a" else None) [1;2;3;4] = Some "a"
  find_map (fun x -> if x=3 then Some "a" else None) [1;2;4;5] = None
*)

let filterAndMap f l =
  let rec recurse acc l = match l with
    | [] -> reverse acc
    | x::l' ->
      let acc' = match f x with | None -> acc | Some y -> y::acc in
      recurse acc' l'
  in recurse [] l

(*$=
  ["2"; "4"] \
    (filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
      [1;2;3;4;5])
  [ "2"; "4"; "6" ] \
    (filter_map (fun x -> if x mod 2 = 0 then Some (string_of_int x) else None) \
      [ 1; 2; 3; 4; 5; 6 ])
*)

let includes ?(eq=(=)) x l =
  let rec search eq x l = match l with
    | [] -> false
    | y::l' -> eq x y || search eq x l'
  in search eq x l

let addUnlessDuplicate ?(eq=(=)) x l =
  if includes ~eq x l then l else x::l

let removeFirst ?(eq=(=)) x l =
  let rec removeFirst ~eq x acc l = match l with
    | [] -> assert false
    | y :: tl when eq x y -> reverseAndAppend acc tl
    | y :: tl -> removeFirst ~eq x (y::acc) tl
  in
  if includes ~eq x l then removeFirst ~eq x [] l else l

(*$Q
  Q.(pair int (list int)) (fun (x,l) -> \
    remove_one x (add_nodup x l) = l)
  Q.(pair int (list int)) (fun (x,l) -> \
    mem x l || List.length (add_nodup x l) = List.length l + 1)
  Q.(pair int (list int)) (fun (x,l) -> \
    not (mem x l) || List.length (remove_one x l) = List.length l - 1)
*)

let subset ?(eq=(=)) l1 l2 =
  List.for_all
    (fun t -> includes ~eq t l2)
    l1

let unique ?(eq=(=)) l =
  let rec unique eq acc l = match l with
    | [] -> reverse acc
    | x::xs when exists (eq x) xs -> unique eq acc xs
    | x::xs -> unique eq (x::acc) xs
  in unique eq [] l

(*$T
  uniq [1;1;2;2;3;4;4;2;4;1;5] |> List.sort Pervasives.compare = [1;2;3;4;5]
*)

let union ?(eq=(=)) l1 l2 =
  let rec union eq acc l1 l2 = match l1 with
    | [] -> List.rev_append acc l2
    | x::xs when includes ~eq x l2 -> union eq acc xs l2
    | x::xs -> union eq (x::acc) xs l2
  in union eq [] l1 l2

(*$T
  union [1;2;4] [2;3;4;5] = [1;2;3;4;5]
*)

let intersection ?(eq=(=)) l1 l2 =
  let rec intersection eq acc l1 l2 = match l1 with
    | [] -> List.rev acc
    | x::xs when includes ~eq x l2 -> intersection eq (x::acc) xs l2
    | _::xs -> intersection eq acc xs l2
  in intersection eq [] l1 l2

(*$T
  inter [1;2;4] [2;3;4;5] = [2;4]
*)
