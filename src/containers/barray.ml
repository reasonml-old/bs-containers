
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Array utils} *)

include Array

let makeMatrix = Array.make_matrix

external makeFloat: int -> float array = "caml_make_float_vect"

let slice = sub

let toList = to_list

let fromList = of_list

let foldLeft = fold_left

let foldRight = fold_right

let stableSort = stable_sort

let fastSort = fast_sort

external unsafeGet : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafeSet : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

(*$T
  let st = Random.State.make [||] in let a = 0--10000 in \
  let b = Array.copy a in shuffle_with st a; a <> b
*)

(** {2 Arrays} *)

type 'a t = 'a array

let empty = [| |]

let map = Array.map

let map2 f a b =
  if Array.length a <> Array.length b then invalid_arg "map2";
  Array.init (Array.length a) (fun i -> f (Array.unsafe_get a i) (Array.unsafe_get b i))

let length = Array.length

let get = Array.get

let getSafe a i =
  if i>=0 && i<Array.length a
  then Some (Array.unsafe_get a i)
  else None

let set = Array.set

let fold ~f ~acc a = Array.fold_left f acc a

let foldi ~f ~acc a =
  let rec aux acc i =
    if i = Array.length a then acc else aux (f acc i a.(i)) (i+1)
  in
  aux acc 0

let foldWhile f acc a =
  let rec fold_while_i f acc i =
    if i < Array.length a then
      let acc, cont = f acc a.(i) in
      match cont with
      | `Stop -> acc
      | `Continue -> fold_while_i f acc (i+1)
    else acc
  in fold_while_i f acc 0

let iter ~f = Array.iter f

let iteri = Array.iteri

let blit = Array.blit

let reverseInPlace a =
  let len = Array.length a in
  if len>0 then (
    for k = 0 to (len-1)/2 do
      let t = a.(k) in
      a.(k) <- a.(len-1-k);
      a.(len-1-k) <- t;
    done
  )

(* This sort algorithm is taken from https://github.com/ocaml/ocaml/blob/trunk/stdlib/array.ml *)
(* Ideally we should replace it with introsrt *)
exception Bottom of int
let sort (cmp:'a Comparator.t) (a:'a array) =
  let maxson l i =
    let i31 = i+i+i+1 in
    let x = ref i31 in
    if i31+2 < l then begin
      if cmp (get a i31) (get a (i31+1)) == Ordering.Less then x := i31+1;
      if cmp (get a !x) (get a (i31+2)) == Ordering.Less then x := i31+2;
      !x
    end else
      if i31+1 < l && cmp (get a i31) (get a (i31+1)) == Ordering.Less
      then i31+1
      else if i31 < l then i31 else raise (Bottom i)
  in
  let rec trickledown l i e =
    let j = maxson l i in
    if cmp (get a j) e == Ordering.Greater then begin
      set a i (get a j);
      trickledown l j e;
    end else begin
      set a i e;
    end;
  in
  let trickle l i e = try trickledown l i e with Bottom i -> set a i e in
  let rec bubbledown l i =
    let j = maxson l i in
    set a i (get a j);
    bubbledown l j
  in
  let bubble l i = try bubbledown l i with Bottom i -> i in
  let rec trickleup i e =
    let father = (i - 1) / 3 in
    assert (i <> father);
    if cmp (get a father) e == Ordering.Less then begin
      set a i (get a father);
      if father > 0 then trickleup father e else set a 0 e;
    end else begin
      set a i e;
    end;
  in
  let l = length a in
  for i = (l + 1) / 3 - 1 downto 0 do trickle l i (get a i); done;
  for i = l - 1 downto 2 do
    let e = (get a i) in
    set a i (get a 0);
    trickleup (bubble i 0) e;
  done;
  if l > 1 then (let e = (get a 1) in set a 1 (get a 0); set a 0 e)

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

let rev a =
  let b = Array.copy a in
  reverseInPlace b;
  b

let rec find_aux f a i =
  if i = Array.length a then None
  else match f i a.(i) with
    | Some _ as res -> res
    | None -> find_aux f a (i+1)

let find ~f a =
  find_aux (fun _ -> f ) a 0

let findi f a =
  find_aux f a 0

let find_idx p a =
  find_aux (fun i x -> if p x then Some (i,x) else None) a 0

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

let bsearch ?(cmp=Pervasives.compare) k a =
  let rec aux i j =
    if i > j
    then `Just_after j
    else
      let middle = i + (j - i) / 2 in (* avoid overflow *)
      match cmp k a.(middle) with
      | 0 -> `At middle
      | n when n<0 -> aux i (middle - 1)
      | _ -> aux (middle + 1) j
  in
  let n = Array.length a in
  if n=0 then `Empty
  else match cmp a.(0) k, cmp a.(n-1) k with
    | c, _ when c>0 -> `All_bigger
    | _, c when c<0 -> `All_lower
    | _ -> aux 0 (n-1)

let (>>=) a f = flatMap f a

let (>>|) a f = map f a

let (>|=) a f = map f a

let forAll ~f a =
  let rec aux i =
    i = Array.length a || (f a.(i) && aux (i+1))
  in
  aux 0

let exists ~f a =
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

let _iter2 f a b i j ~len =
  for o = 0 to len-1 do
    f (Array.get a (i+o)) (Array.get b (j+o))
  done

let _fold2 f acc a b i j ~len =
  let rec aux acc o =
    if o=len then acc
    else
      let acc = f acc (Array.get a (i+o)) (Array.get b (j+o)) in
      aux acc (o+1)
  in
  aux acc 0

let iter2 f a b =
  if length a <> length b then invalid_arg "iter2";
  _iter2 f a b 0 0 ~len:(Array.length a)

let fold2 f acc a b =
  if length a <> length b then invalid_arg "fold2";
  _fold2 f acc a b 0 0 ~len:(Array.length a)

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

(** all the elements of a, but the i-th, into a list *)
let exceptIndex a i =
  foldi
    ~f:(fun acc j elt -> if i = j then acc else elt::acc)
    ~acc:[] a

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

let toIterator a =
  let rec aux i () = 
    if i < length a
    then 
      let x = unsafe_get a i in
      Iterator.Cons (x, aux(i + 1))
    else Iterator.Nil
    in 
    aux 0

let fromIterator i =
  of_list @@ Iterator.foldLeft (fun acc item -> acc @ [item]) [] i


(** {2 Generic Functions} *)

module type MONO_ARRAY = sig
  type elt
  type t

  val length : t -> int

  val get : t -> int -> elt

  val set : t -> int -> elt -> unit
end

(* Dual Pivot Quicksort (Yaroslavskiy)
   from "average case analysis of Java 7's Dual Pivot Quicksort" *)
module SortGeneric(A : MONO_ARRAY) = struct
  module Rand = Random.State

  let seed_ = [|123456|]

  type state = {
    mutable l: int; (* left pointer *)
    mutable g: int; (* right pointer *)
    mutable k: int;
  }

  let rand_idx_ rand i j = i + Rand.int rand (j-i)

  let swap_ a i j =
    if i=j then ()
    else (
      let tmp = A.get a i in
      A.set a i (A.get a j);
      A.set a j tmp
    )

  let sort ~cmp a =
    let rec insert_ a i k =
      if k<i then ()
      else if cmp (A.get a k) (A.get a (k+1)) > 0 then (
        swap_ a k (k+1);
        insert_ a i (k-1)
      )
    in
    (* recursive part of insertion sort *)
    let rec sort_insertion_rec a i j k =
      if k<j then (
        insert_ a i (k-1);
        sort_insertion_rec a i j (k+1)
      )
    in
    (* insertion sort, for small slices *)
    let sort_insertion a i j =
      if j-i > 1 then sort_insertion_rec a i j (i+1)
    in
    let rand = Rand.make seed_ in
    (* sort slice.
       There is a chance that the two pivots are equal, but it's unlikely. *)
    let rec sort_slice_ ~st a i j =
      if j-i>10 then (
        st.l <- i;
        st.g <- j-1;
        st.k <- i;
        (* choose pivots *)
        let p = A.get a (rand_idx_ rand i j) in
        let q = A.get a (rand_idx_ rand i j) in
        (* invariant: st.p <= st.q, swap them otherwise *)
        let p, q = if cmp p q > 0 then q, p else p, q in
        while st.k <= st.g do
          let cur = A.get a st.k in
          if cmp cur p < 0 then (
            (* insert in leftmost band *)
            if st.k <> st.l then swap_ a st.k st.l;
            st.l <- st.l + 1
          ) else if cmp cur q > 0 then (
            (* insert in rightmost band *)
            while st.k < st.g && cmp (A.get a st.g) q > 0 do
              st.g <- st.g - 1
            done;
            swap_ a st.k st.g;
            st.g <- st.g - 1;
            (* the element swapped from the right might be in the first situation.
               that is, < p  (we know it's <= q already) *)
            if cmp (A.get a st.k) p < 0 then (
              if st.k <> st.l then swap_ a st.k st.l;
              st.l <- st.l + 1
            )
          );
          st.k <- st.k + 1
        done;
        (* save values before recursing *)
        let l = st.l and g = st.g and sort_middle = cmp p q < 0 in
        sort_slice_ ~st a i l;
        if sort_middle then sort_slice_ ~st a l (g+1);
        sort_slice_ ~st a (g+1) j;
      ) else sort_insertion a i j
    in
    if A.length a > 0 then (
      let st = { l=0; g=A.length a; k=0; } in
      sort_slice_ ~st a 0 (A.length a)
    )
end


let sort_generic (type arr)(type elt)
    (module A : MONO_ARRAY with type t = arr and type elt = elt)
    ?(cmp=Pervasives.compare) a
  =
  let module S = SortGeneric(A) in
  S.sort ~cmp a

let isEmpty a = length a = 0

let mem p a e =
  let l = length a in
  let rec loop i =
    if i = l then false
    else (
      if p a.(i) e then true
      else loop (i + 1)
    ) in
  loop 0

let count ~f arr =
  foldLeft (fun acc ele -> if f ele then acc + 1 else acc) 0 arr