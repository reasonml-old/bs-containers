
(* This file is free software, part of containers. See file "license" for more details. *)

(*
TODO:

- [x] Use `camelCase` instead of `snake_case`
- [x] Get rid of scary monadic and math-y terminology
- [x] Follow the conventions used in the `Js.*` modules
- [ ] Type functions correctly, e.g. `compare` should return a proper variant, not `0`, `1` or `-1`
- [x] Remove operators and aliases, e.g. `pure` as an alias for `return`
- [ ] Remove the use of exceptions for internal logic
- [x] Replace `Format.printf` calls, they pull in a lot stuff for little benefit
- [ ] Document everything properly, with examples
- [ ] Add tests for everything
*)

(** {1 complements to list} *)

(*$inject
  let lsort l = List.sort Pervasives.compare l
*)

include List

let head = List.hd
let tail = List.tl
let forAll = List.for_all
let findAll = List.find_all
let reduceLeft = List.fold_left
let forEach = List.iter
let reverse = List.rev
let reverseAndAppend = List.rev_append

type 'a t = 'a list

let empty = []

let isEmpty = function
  | [] -> true
  | _::_ -> false

(* max depth for direct recursion *)
let direct_depth_default_ = 1000

let map f l =
  let rec direct f i l = match l with
    | [] -> []
    | [x] -> [f x]
    | [x1;x2] -> let y1 = f x1 in [y1; f x2]
    | [x1;x2;x3] -> let y1 = f x1 in let y2 = f x2 in [y1; y2; f x3]
    | _ when i=0 -> List.rev (List.rev_map f l)
    | x1::x2::x3::x4::l' ->
      let y1 = f x1 in
      let y2 = f x2 in
      let y3 = f x3 in
      let y4 = f x4 in
      y1 :: y2 :: y3 :: y4 :: direct f (i-1) l'
  in
  direct f direct_depth_default_ l

(*$Q
  (Q.list Q.small_int) (fun l -> \
    let f x = x+1 in \
    List.rev (List.rev_map f l) = map f l)
*)

let direct_depth_append_ = 10_000

let cons x l = x::l

let append l1 l2 =
  let rec direct i l1 l2 = match l1 with
    | [] -> l2
    | _ when i=0 -> safe l1 l2
    | x::l1' -> x :: direct (i-1) l1' l2
  and safe l1 l2 =
    List.rev_append (List.rev l1) l2
  in
  match l1 with
  | [] -> l2
  | [x] -> x::l2
  | [x;y] -> x::y::l2
  | _ -> direct direct_depth_append_ l1 l2

let (@) = append

(*$T
  [1;2;3] @ [4;5;6] = [1;2;3;4;5;6]
  (1-- 10_000) @ (10_001 -- 20_000) = 1 -- 20_000
*)

let direct_depth_filter_ = 10_000

let filter p l =
  let rec direct i p l = match l with
    | [] -> []
    | _ when i=0 -> safe p l []
    | x::l' when not (p x) -> direct i p l'
    | x::l' -> x :: direct (i-1) p l'
  and safe p l acc = match l with
    | [] -> List.rev acc
    | x::l' when not (p x) -> safe p l' acc
    | x::l' -> safe p l' (x::acc)
  in
  direct direct_depth_filter_ p l

(*$= & ~printer:CCInt.to_string
  500 (filter (fun x->x mod 2 = 0) (1 -- 1000) |> List.length)
  50_000 (filter (fun x->x mod 2 = 0) (1 -- 100_000) |> List.length)
  500_000 (filter (fun x->x mod 2 = 0) (1 -- 1_000_000) |> List.length)
*)

let reduceRight f l acc =
  let rec direct i f l acc = match l with
    | [] -> acc
    | _ when i=0 -> safe f (List.rev l) acc
    | x::l' ->
      let acc = direct (i-1) f l' acc in
      f x acc
  and safe f l acc = match l with
    | [] -> acc
    | x::l' ->
      let acc = f x acc in
      safe f l' acc
  in
  direct direct_depth_default_ f l acc

(*$T
  fold_right (+) (1 -- 1_000_000) 0 = \
    List.fold_left (+) 0 (1 -- 1_000_000)
*)

(*$Q
  (Q.list Q.small_int) (fun l -> \
    l = fold_right (fun x y->x::y) l [])
*)

let rec reduceWhile f acc = function
  | [] -> acc
  | e::l -> let acc, cont = f acc e in
    match cont with
    | `Stop -> acc
    | `Continue -> reduceWhile f acc l

(*$T
  fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 [true;true;false;true] = 2
*)

let init len f =
  let rec init_rec acc i f =
    if i=0 then f i :: acc
    else init_rec (f i :: acc) (i-1) f
  in
  if len<0 then invalid_arg "init"
  else if len=0 then []
  else init_rec [] (len-1) f

(*$T
  init 0 (fun _ -> 0) = []
  init 1 (fun x->x) = [0]
  init 1000 (fun x->x) = 0--999
*)

let rec compare f l1 l2 = match l1, l2 with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | x1::l1', x2::l2' ->
    let c = f x1 x2 in
    if c <> 0 then c else compare f l1' l2'

let rec equal f l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x1::l1', x2::l2' -> f x1 x2 && equal f l1' l2'

(*$T
  equal CCInt.equal (1--1_000_000) (1--1_000_000)
*)

let flatMap f l =
  let rec aux f l kont = match l with
    | [] -> kont []
    | x::l' ->
      let y = f x in
      let kont' tail = match y with
        | [] -> kont tail
        | [x] -> kont (x :: tail)
        | [x;y] -> kont (x::y::tail)
        | l -> kont (append l tail)
      in
      aux f l' kont'
  in
  aux f l (fun l->l)

(*$T
  flat_map (fun x -> [x+1; x*2]) [10;100] = [11;20;101;200]
  List.length (flat_map (fun x->[x]) (1--300_000)) = 300_000
*)

let flatten l = fold_right append l []

(*$T
  flatten [[1]; [2;3;4]; []; []; [5;6]] = 1--6
  flatten (init 300_001 (fun x->[x])) = 0--300_000
*)

let of_ x = [x]

let take n l =
  let rec direct i n l = match l with
    | [] -> []
    | _ when i=0 -> safe n [] l
    | x::l' ->
      if n > 0
      then x :: direct (i-1) (n-1) l'
      else []
  and safe n acc l = match l with
    | [] -> List.rev acc
    | _ when n=0 -> List.rev acc
    | x::l' -> safe (n-1) (x::acc) l'
  in
  direct direct_depth_default_ n l

(*$T
  take 2 [1;2;3;4;5] = [1;2]
  take 10_000 (range 0 100_000) |> List.length = 10_000
  take 10_000 (range 0 2_000) = range 0 2_000
  take 300_000 (1 -- 400_000) = 1 -- 300_000
*)

(*$Q
  (Q.pair (Q.list Q.small_int) Q.int) (fun (l,i) -> \
    let i = abs i in \
    let l1 = take i l in \
    List.length l1 <= i && ((List.length l1 = i) = (List.length l >= i)))
*)

let rec skip n l = match l with
  | [] -> []
  | _ when n=0 -> l
  | _::l' -> skip (n-1) l'

let takeWhile p l =
  let rec direct i p l = match l with
    | [] -> []
    | _ when i=0 -> safe p [] l
    | x :: l' ->
      if p x then x :: direct (i-1) p l' else []
  and safe p acc l = match l with
    | [] -> List.rev acc
    | x :: l' ->
      if p x then safe p (x::acc) l' else List.rev acc
  in
  direct direct_depth_default_ p l

(*$T
  take_while (fun x->x<10) (1 -- 20) = (1--9)
  take_while (fun x->x <> 0) [0;1;2;3] = []
  take_while (fun _ -> true) [] = []
  take_while (fun _ -> true) (1--10) = (1--10)
*)

(*$Q
  Q.(pair (fun1 small_int bool) (list small_int)) (fun (f,l) -> \
    let l1 = take_while f l in \
    List.for_all f l1)
*)

let rec skipWhile p l = match l with
  | [] -> []
  | x :: l' -> if p x then skipWhile p l' else l

(*$Q
  Q.(pair (fun1 small_int bool) (list small_int)) (fun (f,l) -> \
    take_while f l @ drop_while f l = l)
*)

let splitAt n l = take n l, skip n l

(*$Q
  (Q.pair (Q.list Q.small_int) Q.int) (fun (l,i) -> \
    let i = abs i in \
    let l1, l2 = take_drop i l in \
    l1 @ l2 = l )
*)

let last n l =
  let len = List.length l in
  if len < n then l else skip (len-n) l

let headOption = function
  | [] -> None
  | x::_ -> Some x

let rec lastOption = function
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> lastOption tail

(*$= & ~printer:Q.Print.(option int)
  (Some 1) (head_opt [1;2;3])
  (Some 1) (head_opt [1])
  None (head_opt [])
  (Some 3) (last_opt [1;2;3])
  (Some 1) (last_opt [1])
  None (last_opt [])
*)

let rec find p l = match l with
  | [] -> None
  | x :: _ when p x -> Some x
  | _ :: tl -> find p tl

let findOrRaise p l = match find p l with
  | None -> raise Not_found
  | Some x -> x

(*$T
  find_pred ((=) 4) [1;2;5;4;3;0] = Some 4
  find_pred (fun _ -> true) [] = None
  find_pred (fun _ -> false) (1 -- 10) = None
  find_pred (fun x -> x < 10) (1 -- 9) = Some 1
*)

let removeAll ?(eq=(=)) ~x l =
  let rec remove' eq x acc l = match l with
    | [] -> List.rev acc
    | y :: tail when eq x y -> remove' eq x acc tail
    | y :: tail -> remove' eq x (y::acc) tail
  in
  remove' eq x [] l

(*$T
  remove ~x:1 [2;1;3;3;2;1] = [2;3;3;2]
  remove ~x:10 [1;2;3] = [1;2;3]
*)

let mapi f l =
  let r = ref 0 in
  map
    (fun x ->
       let y = f !r x in
       incr r; y
    ) l

(*$T
  mapi (fun i x -> i*x) [10;10;10] = [0;10;20]
*)

let forEachi f l =
  let rec aux f i l = match l with
    | [] -> ()
    | x::l' -> f i x; aux f (i+1) l'
  in aux f 0 l

let reducei f acc l =
  let rec foldi f acc i l = match l with
    | [] -> acc
    | x::l' ->
      let acc = f acc i x in
      foldi f acc (i+1) l'
  in
  foldi f acc 0 l

let rec getOrRaise i l = match l with
  | [] -> raise Not_found
  | x::_ when i=0 -> x
  | _::l' -> getOrRaise (i-1) l'

let get i l =
  try Some (getOrRaise i l)
  with Not_found -> None

(*$T
  get_at_idx 0 (range 0 10) = Some 0
  get_at_idx 5 (range 0 10) = Some 5
  get_at_idx 11 (range 0 10) = None
  get_at_idx 0 [] = None
*)

let set i x l0 =
  let rec aux l acc i = match l with
    | [] -> l0
    | _::l' when i=0 -> List.rev_append acc (x::l')
    | y::l' ->
      aux l' (y::acc) (i-1)
  in
  aux l0 [] i

(*$T
  set_at_idx 0 10 [1;2;3] = [10;2;3]
  set_at_idx 4 10 [1;2;3] = [1;2;3]
  set_at_idx 1 10 [1;2;3] = [1;10;3]
*)

let insert i x l =
  let rec aux l acc i x = match l with
    | [] -> List.rev_append acc [x]
    | y::l' when i=0 -> List.rev_append acc (x::y::l')
    | y::l' ->
      aux l' (y::acc) (i-1) x
  in
  aux l [] i x

(*$T
  insert_at_idx 0 10 [1;2;3] = [10;1;2;3]
  insert_at_idx 4 10 [1;2;3] = [1;2;3;10]
  insert_at_idx 1 10 [1;2;3] = [1;10;2;3]
*)

let removeAt i l0 =
  let rec aux l acc i = match l with
    | [] -> l0
    | _::l' when i=0 -> List.rev_append acc l'
    | y::l' ->
      aux l' (y::acc) (i-1)
  in
  aux l0 [] i

(*$T
  remove_at_idx 0 [1;2;3;4] = [2;3;4]
  remove_at_idx 3 [1;2;3;4] = [1;2;3]
  remove_at_idx 5 [1;2;3;4] = [1;2;3;4]
*)

let rangeBy ~step i j =
  let rec range i j acc =
    if i=j then i::acc else range i (j-step) (j::acc)
  in
  if step = 0 then
    raise (Invalid_argument "CCList.range_by")
  else if (if step > 0 then i>j else i<j) then
    []
  else
    range i ((j-i)/step*step + i)  []

(* note: the last test checks that no error occurs due to overflows. *)
(*$T
  range_by ~step:1   0 0 = [0]
  range_by ~step:1   5 0 = []
  range_by ~step:2   1 0 = []
  range_by ~step:2   0 4 = [0;2;4]
  range_by ~step:2   0 5 = [0;2;4]
  range_by ~step:~-1 0 0 = [0]
  range_by ~step:~-1 0 5 = []
  range_by ~step:~-2 0 1 = []
  range_by ~step:~-2 5 1 = [5;3;1]
  range_by ~step:~-2 5 0 = [5;3;1]
  range_by ~step:max_int 0 2 = [0]
*)

(*$Q
  Q.(pair small_int small_int) (fun (i,j) -> \
    let i = min i j and j = max i j in \
    range_by ~step:1 i j = range i j)
*)

let range i j =
  let rec up i j acc =
    if i=j then i::acc else up i (j-1) (j::acc)
  and down i j acc =
    if i=j then i::acc else down i (j+1) (j::acc)
  in
  if i<=j then up i j [] else down i j []

(*$T
  range 0 5 = [0;1;2;3;4;5]
  range 0 0 = [0]
  range 5 2 = [5;4;3;2]
*)

let range' i j =
  if i<j then range i (j-1)
  else if i=j then []
  else range i (j+1)

(*$T
  range' 0 0 = []
  range' 0 5 = [0;1;2;3;4]
  range' 5 2 = [5;4;3]
*)

let (--) = range

let (--^) = range'

(*$T
  append (range 0 100) (range 101 1000) = range 0 1000
  append (range 1000 501) (range 500 0) = range 1000 0
*)

(*$Q
  Q.(pair small_int small_int) (fun (a,b) -> \
    let l = (a--^b) in not (List.mem b l))
*)

let replicate i x =
  let rec aux acc i =
    if i = 0 then acc
    else aux (x::acc) (i-1)
  in aux [] i

let repeat i l =
  let l' = List.rev l in
  let rec aux acc i =
    if i = 0 then List.rev acc
    else aux (reverseAndAppend l' acc) (i-1)
  in aux [] i

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit

let toSequence l k = List.iter k l
let fromSequence seq =
  let l = ref [] in
  seq (fun x -> l := x :: !l);
  List.rev !l

module Infix = struct
  let (@) = (@)
  let (--) = (--)
  let (--^) = (--^)
end
