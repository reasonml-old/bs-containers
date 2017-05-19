
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic String Utils} *)

type t = string
type 'a gen = unit -> 'a option
type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

module type S = sig
  type t

  val length : t -> int

  val blit : t -> int -> Bytes.t -> int -> int -> unit
  (** Similar to {!String.blit}.
      Compatible with the [-safe-string] option.
      @raise Invalid_argument if indices are not valid *)

  val reduce : ('a -> char -> 'a) -> 'a -> t -> 'a

  (** {2 Conversions} *)

  val toSequence : t -> char sequence
  val toList : t -> char list
end

let equal (a:string) b = a=b

let compare = String.compare

let hash s = Hashtbl.hash s

let make = String.make

let makeWithInit = String.init

let substring s ~from ~length = String.sub s from length

let concat = String.concat

let blit = String.blit

let uncapitalize = String.uncapitalize
let capitalize = String.capitalize
let lowercase = String.lowercase
let uppercase = String.uppercase
let escaped = String.escaped
let trim = String.trim

(*let init n f =
  let buf = Bytes.init n f in
  Bytes.unsafe_to_string buf*)

external get : string -> int -> char = "%string_safe_get"

let length = String.length

let forEach = String.iter
let map = String.map
let forEachWithIndex = String.iteri
let mapWithIndex = String.mapi

let reverse s =
  let n = length s in
  makeWithInit n (fun i -> s.[n-i-1])

let rec _toList s acc i len =
  if len=0 then List.rev acc
  else _toList s (s.[i]::acc) (i+1) (len-1)

let _isSubstring ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else sub.[i+k] = s.[j+k] && check (k+1)
  in
  j+len <= length s && check 0

let isSubstring ~sub i s j ~len =
  if i+len > length sub then invalid_arg "CCString.is_sub";
  _isSubstring ~sub i s j ~len

type _ direction =
  | Direct : [`Direct] direction
  | Reverse : [`Reverse] direction

let indexOf s ?from c =
  match from with
  | Some i -> String.index_from s i c
  | None -> String.index s c

let lastIndexOf s ?from c =
  match from with
  | Some i -> String.rindex_from s i c
  | None -> String.rindex s c

let contains s ?from ?to_ c =
  match from, to_ with
  | None, None -> String.contains s c
  | Some from, None -> String.contains_from s from c
  | None, Some to_ -> String.rcontains_from s to_ c
  | Some from, Some to_ -> 
    String.contains_from s from c && String.rcontains_from s to_ c (* TODO: Make less terribly inefficient *)
  
(* we follow https://en.wikipedia.org/wiki/Knuth–Morris–Pratt_algorithm *)
module Find = struct
  type 'a kmp_pattern = {
    failure : int array;
    str : string;
  }
  (* invariant: [length failure = length str].
     We use a phantom type to avoid mixing the directions. *)

  let kmp_pattern_length p = length p.str

  (* access the [i]-th element of [s] according to direction [dir] *)
  let get_
    : type a. dir:a direction -> string -> int -> char
    = fun ~dir -> match dir with
      | Direct -> get
      | Reverse -> (fun s i -> s.[length s - i - 1])

  let kmp_compile_
    : type a. dir:a direction -> string -> a kmp_pattern
    = fun ~dir str ->
      let len = length str in
      let get = get_ ~dir in (* how to read elements of the string *)
      match len with
      | 0 -> {failure=[| |]; str;}
      | 1 -> {failure=[| -1 |]; str;}
      | _ ->
        (* at least 2 elements, the algorithm can work *)
        let failure = Array.make len 0 in
        failure.(0) <- -1;
        (* i: current index in str *)
        let i = ref 2 in
        (* j: index of candidate substring *)
        let j = ref 0 in
        while !i < len do
          match !j with
          | _ when get str (!i-1) = get str !j ->
            (* substring starting at !j continues matching current char *)
            incr j;
            failure.(!i) <- !j;
            incr i;
          | 0 ->
            (* back to the beginning *)
            failure.(!i) <- 0;
            incr i;
          | _ ->
            (* fallback for the prefix string *)
            assert (!j > 0);
            j := failure.(!j)
        done;
        (* Format.printf "{@[failure:%a, str:%s@]}@." CCFormat.(array int) failure str; *)
        { failure; str; }

  let kmp_compile s = kmp_compile_ ~dir:Direct s
  let kmp_rcompile s = kmp_compile_ ~dir:Reverse s

  (* proper search function.
     [i] index in [s]
     [j] index in [pattern]
     [len] length of [s] *)
  let kmp_find ~pattern s idx =
    let len = length s in
    let i = ref idx in
    let j = ref 0 in
    let pat_len = kmp_pattern_length pattern in
    while !j < pat_len && !i + !j < len do
      let c = get s (!i + !j) in
      let expected = get pattern.str !j in
      if c = expected
      then (
        (* char matches *)
        incr j;
      ) else (
        let fail_offset = pattern.failure.(!j) in
        if fail_offset >= 0
        then (
          assert (fail_offset < !j);
          (* follow the failure link *)
          i := !i + !j - fail_offset;
          j := fail_offset
        ) else (
          (* beginning of pattern *)
          j := 0;
          incr i
        )
      )
    done;
    if !j = pat_len
    then !i
    else -1

  (* proper search function, from the right.
     [i] index in [s]
     [j] index in [pattern]
     [len] length of [s] *)
  let kmp_rfind ~pattern s idx =
    let len = length s in
    let i = ref (len - idx - 1) in
    let j = ref 0 in
    let pat_len = kmp_pattern_length pattern in
    while !j < pat_len && !i + !j < len do
      let c = get s (len - !i - !j - 1) in
      let expected = get pattern.str (length pattern.str - !j - 1) in
      if c = expected
      then (
        (* char matches *)
        incr j;
      ) else (
        let fail_offset = pattern.failure.(!j) in
        if fail_offset >= 0
        then (
          assert (fail_offset < !j);
          (* follow the failure link *)
          i := !i + !j - fail_offset;
          j := fail_offset
        ) else (
          (* beginning of pattern *)
          j := 0;
          incr i
        )
      )
    done;
    (* adjust result: first, [res = string.length s - res -1] to convert
       back to real indices; then, what we got is actually the position
       of the end of the pattern, so we subtract the [length of the pattern -1]
       to obtain the real result. *)
    if !j = pat_len
    then len - !i - kmp_pattern_length pattern
    else -1

  type 'a pattern =
    | P_char of char
    | P_KMP of 'a kmp_pattern

  let pattern_length = function
    | P_char _ -> 1
    | P_KMP p -> kmp_pattern_length p

  let compile sub : [`Direct] pattern =
    if length sub=1
    then P_char sub.[0]
    else P_KMP (kmp_compile sub)

  let compileReversed sub : [`Reverse] pattern =
    if length sub=1
    then P_char sub.[0]
    else P_KMP (kmp_rcompile sub)

  let find ?(start=0) ~(pattern:[`Direct] pattern) s = match pattern with
    | P_char c ->
      (try indexOf s ~from:start c with Not_found -> -1)
    | P_KMP pattern -> kmp_find ~pattern s start

  let findReversed ?start ~(pattern:[`Reverse] pattern) s =
    let start = match start with
      | Some n -> n
      | None -> length s - 1
    in
    match pattern with
    | P_char c ->
      (try lastIndexOf s ~from:start c with Not_found -> -1)
    | P_KMP pattern -> kmp_rfind ~pattern s start
end

let find ?(start=0) ~sub =
  let pattern = Find.compile sub in
  fun s -> Find.find ~pattern s ~start

let findAll ?(start=0) ~sub =
  let pattern = Find.compile sub in
  fun s ->
    let i = ref start in
    fun () ->
      let res = Find.find ~pattern s ~start:!i in
      if res = ~-1 then None
      else (
        i := res + 1; (* possible overlap *)
        Some res
      )

let findAllList ?start ~sub s =
  let rec aux acc g = match g () with
    | None -> List.rev acc
    | Some i -> aux (i::acc) g
  in
  aux [] (findAll ?start ~sub s)

let includes ?start ~sub s = find ?start ~sub s >= 0

let findReversed ~sub =
  let pattern = Find.compileReversed sub in
  fun s -> Find.findReversed ~pattern s ~start:(length s-1)

(* Replace substring [s.[pos]....s.[pos+len-1]] by [by] in [s] *)
let replace_at_ ~pos ~len ~by s =
  let b = Buffer.create (length s + length by - len) in
  Buffer.add_substring b s 0 pos;
  Buffer.add_string b by;
  Buffer.add_substring b s (pos+len) (length s - pos - len);
  Buffer.contents b

let replace ?(which=`All) ~sub ~by s =
  if sub="" then invalid_arg "CCString.replace";
  match which with
  | `Left ->
    let i = find ~sub s ~start:0 in
    if i>=0 then replace_at_ ~pos:i ~len:(length sub) ~by s else s
  | `Right ->
    let i = findReversed ~sub s in
    if i>=0 then replace_at_ ~pos:i ~len:(length sub) ~by s else s
  | `All ->
    (* compile search pattern only once *)
    let pattern = Find.compile sub in
    let b = Buffer.create (length s) in
    let start = ref 0 in
    while !start < length s do
      let i = Find.find ~pattern s ~start:!start in
      if i>=0 then (
        (* between last and cur occurrences *)
        Buffer.add_substring b s !start (i- !start);
        Buffer.add_string b by;
        start := i + length sub
      ) else (
        (* add remainder *)
        Buffer.add_substring b s !start (length s - !start);
        start := length s (* stop *)
      )
    done;
    Buffer.contents b

module Split = struct
  type split_state =
    | SplitStop
    | SplitAt of int (* previous *)

  let rec _split ~by s state = match state with
    | SplitStop -> None
    | SplitAt prev -> _split_search ~by s prev
  and _split_search ~by s prev =
    let j = Find.find ~pattern:by s ~start:prev in
    if j < 0
    then Some (SplitStop, prev, length s - prev)
    else Some (SplitAt (j+Find.pattern_length by), prev, j-prev)

  let _tuple3 x y z = x,y,z

  let _mkgen ~by s k =
    let state = ref (SplitAt 0) in
    let by = Find.compile by in
    fun () ->
      match _split ~by s !state with
      | None -> None
      | Some (state', i, len) ->
        state := state';
        Some (k s i len)

  let _mklist ~by s k =
    let by = Find.compile by in
    let rec build acc state = match _split ~by s state with
      | None -> List.rev acc
      | Some (state', i, len) ->
        build (k s i len ::acc) state'
    in
    build [] (SplitAt 0)

  let list_ ~by s = _mklist ~by s _tuple3

  let listCopy ~by s = _mklist ~by s (fun s i n -> substring s ~from:i ~length:n)

  let _mkklist ~by s k =
    let by = Find.compile by in
    let rec make state () = match _split ~by s state with
      | None -> `Nil
      | Some (state', i, len) ->
        `Cons (k s i len , make state')
    in make (SplitAt 0)

  let _mkseq ~by s f k =
    let by = Find.compile by in
    let rec aux state = match _split ~by s state with
      | None -> ()
      | Some (state', i, len) -> k (f s i len); aux state'
    in aux (SplitAt 0)

  let sequence ~by s = _mkseq ~by s _tuple3
  let sequenceCopy ~by s = _mkseq ~by s (fun s i n -> substring s ~from:i ~length:n)

  let leftOrRaise ~by s =
    let i = find ~sub:by s in
    if i = ~-1 then raise Not_found
    else
      let right = i + length by in
      substring s ~from:0 ~length:i, substring s ~from:right ~length:(length s - right)

  let left ~by s = try Some (leftOrRaise ~by s) with Not_found -> None

  let rightOrRaise ~by s =
    let i = findReversed ~sub:by s in
    if i = ~-1 then raise Not_found
    else
      let right = i + length by in
      substring s ~from:0 ~length:i, substring s ~from:right ~length:(length s - right)

  let right ~by s = try Some (rightOrRaise ~by s) with Not_found -> None
end

let editDistance s1 s2 =
  if length s1 = 0
  then length s2
  else if length s2 = 0
  then length s1
  else if s1 = s2
  then 0
  else begin
    (* distance vectors (v0=previous, v1=current) *)
    let v0 = Array.make (length s2 + 1) 0 in
    let v1 = Array.make (length s2 + 1) 0 in
    (* initialize v0: v0(i) = A(0)(i) = delete i chars from t *)
    for i = 0 to length s2 do
      v0.(i) <- i
    done;
    (* main loop for the bottom up dynamic algorithm *)
    for i = 0 to length s1 - 1 do
      (* first edit distance is the deletion of i+1 elements from s *)
      v1.(0) <- i+1;

      (* try add/delete/replace operations *)
      for j = 0 to length s2 - 1 do
        let cost = if Char.compare (get s1 i) (get s2 j) = 0 then 0 else 1 in
        v1.(j+1) <- min (v1.(j) + 1) (min (v0.(j+1) + 1) (v0.(j) + cost));
      done;

      (* copy v1 into v0 for next iteration *)
      Array.blit v1 0 v0 0 (length s2 + 1);
    done;
    v1.(length s2)
  end

let repeat s n =
  assert (n>=0);
  let len = length s in
  assert(len > 0);
  makeWithInit (len * n) (fun i -> s.[i mod len])

let isPrefix ~pre s =
  length pre <= length s &&
  (let i = ref 0 in
   while !i < length pre && s.[!i] = pre.[!i] do incr i done;
   !i = length pre
  )

let isSuffix ~suf s =
  length suf <= length s &&
  let off = length s - length suf in
  (let i = ref 0 in
   while !i < length suf && s.[off + !i] = suf.[!i] do incr i done;
   !i = length suf
  )

let take n s =
  if n < length s
  then substring s ~from:0 ~length:n
  else s

let drop n s =
  if n < length s
  then substring s ~from:n ~length:(length s - n)
  else ""

let takeDrop n s = take n s, drop n s

let chopSuffix ~suf s =
  if isSuffix ~suf s
  then Some (String.sub s 0 (length s-length suf))
  else None

let chopPrefix ~pre s =
  if isPrefix ~pre s
  then Some (substring s ~from:(length pre) ~length:(length s-length pre))
  else None

external unsafeGetUnchecked : string -> int -> char = "%string_unsafe_get"
external unsafeBlitUnchecked :
  string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string" "noalloc"


let reduce f acc s =
  let rec fold_rec f acc s i =
    if i = length s then acc
    else fold_rec f (f acc s.[i]) s (i+1)
  in fold_rec f acc s 0

let pad ?(side=`Left) ?(char=' ') n s =
  let len_s = length s in
  if len_s >= n then s
  else
    let pad_len = n - len_s in
    match side with
    | `Left -> makeWithInit n (fun i -> if i < pad_len then char else s.[i-pad_len])
    | `Right -> makeWithInit n (fun i -> if i < len_s then s.[i] else char)

let fromChar c = make 1 c

let toSequence s k = forEach k s

let fromSequence seq =
  let b= Buffer.create 32 in
  seq (Buffer.add_char b);
  Buffer.contents b

let toList s = _toList s [] 0 (length s)

let fromList l =
  let buf = Buffer.create (List.length l) in
  List.iter (Buffer.add_char buf) l;
  Buffer.contents buf

let fromArray a =
  makeWithInit (Array.length a) (fun i -> a.(i))

let toArray s =
  Array.init (length s) (fun i -> s.[i])

let lines s = Split.listCopy ~by:"\n" s

let unlines l = concat "\n" l

let set s i c =
  if i < 0 || i >= length s then invalid_arg "CCString.set";
  makeWithInit (length s) (fun j -> if i=j then c else s.[j])

let filterMap f s =
  let buf = Buffer.create (length s) in
  forEach
    (fun c -> match f c with
       | None -> ()
       | Some c' -> Buffer.add_char buf c')
    s;
  Buffer.contents buf

let filter f s =
  let buf = Buffer.create (length s) in
  forEach
    (fun c -> if f c then Buffer.add_char buf c)
    s;
  Buffer.contents buf

let flatMap ?sep f s =
  let buf = Buffer.create (length s) in
  forEachWithIndex
    (fun i c ->
       begin match sep with
         | Some _ when i=0 -> ()
         | None -> ()
         | Some sep -> Buffer.add_string buf sep
       end;
       Buffer.add_string buf (f c)
    ) s;
  Buffer.contents buf

exception MyExit

let forAll p s =
  try forEach (fun c -> if not (p c) then raise MyExit) s; true
  with MyExit -> false

let exists p s =
  try forEach (fun c -> if p c then raise MyExit) s; false
  with MyExit -> true

let map2 f s1 s2 =
  if length s1 <> length s2 then invalid_arg "CCString.map2";
  makeWithInit (length s1) (fun i -> f s1.[i] s2.[i])

let forEach2 f s1 s2 =
  if length s1 <> length s2 then invalid_arg "CCString.iter2";
  for i = 0 to length s1 - 1 do
    f s1.[i] s2.[i]
  done

let forEach2WithIndex f s1 s2 =
  if length s1 <> length s2 then invalid_arg "CCString.iteri2";
  for i = 0 to length s1 - 1 do
    f i s1.[i] s2.[i]
  done

let reduce2 f acc s1 s2 =
  if length s1 <> length s2 then invalid_arg "CCString.fold2";
  let rec fold' acc s1 s2 i =
    if i = length s1 then acc
    else fold' (f acc s1.[i] s2.[i]) s1 s2 (i+1)
  in
  fold' acc s1 s2 0

let forAll2 p s1 s2 =
  try forEach2 (fun c1 c2 -> if not (p c1 c2) then raise MyExit) s1 s2; true
  with MyExit -> false

let exists2 p s1 s2 =
  try forEach2 (fun c1 c2 -> if p c1 c2 then raise MyExit) s1 s2; false
  with MyExit -> true


module Sub = struct
  type t = string * int * int

  let make s i ~len =
    if i<0||len<0||i+len > length s then invalid_arg "CCString.Sub.make";
    s,i,len

  let full s = s, 0, length s

  let copy (s,i,len) = String.sub s i len

  let underlying (s,_,_) = s

  let sub (s,i,len) i' len' =
    if i+i' + len' > i+len then invalid_arg "CCString.Sub.sub";
    (s, i+i',len')

  let length (_,_,l) = l

  let blit (a1,i1,len1) o1 a2 o2 len =
    if o1+len>len1 then invalid_arg "CCString.Sub.blit";
    blit a1 (i1+o1) a2 o2 len

  let reduce f acc (s,i,len) =
    let rec fold_rec f acc s i j =
      if i = j then acc
      else fold_rec f (f acc s.[i]) s (i+1) j
    in fold_rec f acc s i (i+len)

  let toSequence (s,i,len) k =
    for i=i to i+len-1 do k s.[i] done
  let toList (s,i,len) = _toList s [] i len
end
