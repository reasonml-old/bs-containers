
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Array utils} *)

include Array

type 'a t = 'a array

let empty = [||]

let isEmpty a = length a = 0

let makeMatrix = Array.make_matrix

external makeFloat: int -> float array = "caml_make_float_vect"

external unsafe_sub : 'a array -> int -> int -> 'a array = "caml_array_sub"

let length = Array.length

let get i a =
  if i>=0 && i<Array.length a
  then Some (Array.unsafe_get a i)
  else None

let set i arr a =
  if i > 0 && i < Array.length arr
  then arr.(i) <- a
  else ()

let getOrRaise = Array.get

let setOrRaise = Array.set

let equals eq a b =
  let rec aux i =
    if i = Array.length a then true
    else eq a.(i) b.(i) && aux (i+1)
  in
  Array.length a = Array.length b
  &&
  aux 0

let compare cmp a b =
  let rec aux i =
    if i = Array.length a
    then if i = Array.length b then Ordering.Equal else Ordering.Less
    else if i = Array.length b
    then Ordering.Greater
    else
      let c = cmp a.(i) b.(i) in
      if c = Ordering.Equal then aux (i+1) else c
  in
  aux 0

let slice ?start:(s = 0) ?end_:(e = 0) arr =
  if s < 0 || e < 0 || e > length arr then None
  else Some (unsafe_sub arr s (e - s))

let toList = Array.to_list

let fromList = Array.of_list

let toSequence a =
  let rec aux i () = 
    if i < length a
    then 
      let x = unsafe_get a i in
      Sequence.Cons (x, aux(i + 1))
    else Sequence.Nil
  in 
  aux 0

let fromSequence i =
  of_list @@ Sequence.foldLeft (fun acc item -> acc @ [item]) [] i

let map2 f a b =
  if Array.length a <> Array.length b then invalid_arg "map2";
  Array.init (Array.length a) (fun i -> f (Array.unsafe_get a i) (Array.unsafe_get b i))

let reduce = Array.fold_left

let reduceReversed = Array.fold_right

let reduceWithIndex ~f ~acc a =
  let rec aux acc i =
    if i = Array.length a then acc else aux (f acc i a.(i)) (i+1)
  in
  aux acc 0

let reduceWhile f acc a =
  let rec fold_while_i f acc i =
    if i < Array.length a then
      let acc, cont = f acc a.(i) in
      match cont with
      | true -> acc
      | false -> fold_while_i f acc (i+1)
    else acc
  in fold_while_i f acc 0

let forEachWithIndex = Array.iteri

let blit = Array.blit

let _reverseInPlace a =
  let len = Array.length a in
  if len>0 then (
    for k = 0 to (len-1)/2 do
      let t = a.(k) in
      a.(k) <- a.(len-1-k);
      a.(len-1-k) <- t;
    done
  )

let reverse a =
  let b = Array.copy a in
  _reverseInPlace b;
  b

let reverseInPlace a = _reverseInPlace a

(* This sort algorithm is taken from https://github.com/ocaml/ocaml/blob/trunk/stdlib/array.ml *)
(* Ideally we should replace it with introsrt *)
exception Bottom of int
let sort (cmp:'a Comparator.t) (a:'a array) =
  let maxson l i =
    let i31 = i+i+i+1 in
    let x = ref i31 in
    if i31+2 < l then begin
      if cmp (Array.get a i31) (Array.get a (i31+1)) == Ordering.Less then x := i31+1;
      if cmp (Array.get a !x) (Array.get a (i31+2)) == Ordering.Less then x := i31+2;
      !x
    end else
    if i31+1 < l && cmp (Array.get a i31) (Array.get a (i31+1)) == Ordering.Less
    then i31+1
    else if i31 < l then i31 else raise (Bottom i)
  in
  let rec trickledown l i e =
    let j = maxson l i in
    if cmp (Array.get a j) e == Ordering.Greater then begin
      Array.set a i (Array.get a j);
      trickledown l j e;
    end else begin
      Array.set a i e;
    end;
  in
  let trickle l i e = try trickledown l i e with Bottom i -> Array.set a i e in
  let rec bubbledown l i =
    let j = maxson l i in
    Array.set a i (Array.get a j);
    bubbledown l j
  in
  let bubble l i = try bubbledown l i with Bottom i -> i in
  let rec trickleup i e =
    let father = (i - 1) / 3 in
    assert (i <> father);
    if cmp (Array.get a father) e == Ordering.Less then begin
      Array.set a i (Array.get a father);
      if father > 0 then trickleup father e else Array.set a 0 e;
    end else begin
      Array.set a i e;
    end;
  in
  let l = length a in
  for i = (l + 1) / 3 - 1 downto 0 do trickle l i (Array.get a i); done;
  for i = l - 1 downto 2 do
    let e = (Array.get a i) in
    Array.set a i (Array.get a 0);
    trickleup (bubble i 0) e;
  done;
  if l > 1 then (let e = (Array.get a 1) in Array.set a 1 (Array.get a 0); Array.set a 0 e)

let stableSort = Array.stable_sort

let fastSort = Array.fast_sort

let sorted cmp a =
  let b = Array.copy a in
  sort cmp b;
  b

let sortIndices cmp a =
  let len = Array.length a in
  let b = Array.init len (fun k->k) in
  sort (fun k1 k2 -> cmp a.(k1) a.(k2)) b;
  b

let sortRanking cmp a =
  let cmp_int = Comparator.int in
  sortIndices cmp_int (sortIndices cmp a)

let rec find_aux f a i =
  if i = Array.length a then None
  else match f i a.(i) with
    | Some _ as res -> res
    | None -> find_aux f a (i+1)

let find f a =
  find_aux (fun _ -> f ) a 0

let findWithIndex f a =
  find_aux f a 0

let findIndex p a =
  find_aux (fun i x -> if p x then Some (i,x) else None) a 0

let bsearch ?(cmp=Comparator.make Pervasives.compare) k a =
  let rec aux i j =
    if i > j
    then `Just_after j
    else
      let middle = i + (j - i) / 2 in (* avoid overflow *)
      match cmp k a.(middle) with
      | Ordering.Equal -> `At middle
      | Ordering.Less -> aux i (middle - 1)
      | _ -> aux (middle + 1) j
  in
  let n = Array.length a in
  if n=0 then `Empty
  else match cmp a.(0) k, cmp a.(n-1) k with
    | Ordering.Greater, _ -> `All_bigger
    | _, Ordering.Less -> `All_lower
    | _ -> aux 0 (n-1)

let forAll f a =
  let rec aux i =
    i = Array.length a || (f a.(i) && aux (i+1))
  in
  aux 0

let exists f a =
  let rec aux i =
    i <> Array.length a && (f a.(i) || aux (i+1))
  in
  aux 0

let rec _for_all2 p a1 a2 i1 i2 ~len =
  len=0 || (p a1.(i1) a2.(i2) && _for_all2 p a1 a2 (i1+1) (i2+1) ~len:(len-1))

let forAll2 p a b =
  Array.length a = Array.length b
  &&
  _for_all2 p a b 0 0 ~len:(Array.length a)

let rec _exists2 p a1 a2 i1 i2 ~len =
  len>0 && (p a1.(i1) a2.(i2) || _exists2 p a1 a2 (i1+1) (i2+1) ~len:(len-1))

let exists2 p a b =
  _exists2 p a b 0 0 ~len:(min (Array.length a) (Array.length b))

let count f arr =
  reduce (fun acc ele -> if f ele then acc + 1 else acc) 0 arr

let _iter2 f a b i j ~len =
  for o = 0 to len-1 do
    f (Array.get a (i+o)) (Array.get b (j+o))
  done

let iter2 f a b =
  if length a <> length b then invalid_arg "iter2";
  _iter2 f a b 0 0 ~len:(Array.length a)

(* shuffle a[i...j[ using the given int random generator
   See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let _shuffle _rand_int a i j =
  for k = j-1 downto i+1 do
    let l = _rand_int (k+1) in
    let tmp = a.(l) in
    a.(l) <- a.(k);
    a.(k) <- tmp;
  done

let shuffle a =
  _shuffle Random.int a 0 (Array.length a)

let shuffleWith st a =
  _shuffle (Random.State.int st) a 0 (Array.length a)

let filterMap f a =
  let rec aux acc i =
    if i = Array.length a
    then (
      let a' = Array.of_list acc in
      reverseInPlace a';
      a'
    ) else match f a.(i) with
      | None -> aux acc (i+1)
      | Some x -> aux (x::acc) (i+1)
  in aux [] 0

let filter p a =
  filterMap (fun x -> if p x then Some x else None) a

(* append [rev a] in front of [acc] *)
let rec __rev_append_list a acc i =
  if i = Array.length a
  then acc
  else
    __rev_append_list a (a.(i) :: acc) (i+1)

let flatMap f a =
  let rec aux acc i =
    if i = Array.length a
    then (
      let a' = Array.of_list acc in
      reverseInPlace a';
      a'
    )
    else
      let a' = f a.(i) in
      aux (__rev_append_list a' acc 0) (i+1)
  in aux [] 0

(** all the elements of a, but the i-th, into a list *)
let exceptIndex a i =
  reduceWithIndex
    ~f:(fun acc j elt -> if i = j then acc else elt::acc)
    ~acc:[] a

let (--) i j =
  if i<=j
  then
    Array.init (j-i+1) (fun k -> i+k)
  else
    Array.init (i-j+1) (fun k -> i-k)

let (--^) i j =
  if i=j then [| |]
  else if i>j
  then Array.init (i-j) (fun k -> i-k)
  else Array.init (j-i) (fun k -> i+k)


















