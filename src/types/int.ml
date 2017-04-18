
(* This file is free software, part of containers. See file "license" for more details. *)

type t = int

let equals (a:int) b = a=b

let compare = Comparator.int

let hash i = i land max_int

let sign i =
  if i < 0 then -1
  else if i>0 then 1
  else 0

let neg i = -i

(** 
  Implements tail recursive version of the Exponentiation by squaring algorithm
  see [this link](https://en.wikipedia.org/wiki/Exponentiation_by_squaring#Runtime_example:_compute_310)
*)
let pow a b =
  let rec aux result acc = function
    | 1 -> result * acc
    | n ->
      if n mod 2 = 0
      then aux result (acc * acc) (n/2)
      else aux (result * acc) (acc * acc) (n/2)
  in
  match b with
  | 0 -> if a = 0 then raise (Invalid_argument "pow: undefined value 0^0") else 1
  | b when b < 0 -> raise (Invalid_argument "pow: can't raise int to negative power")
  | b -> aux 1 a b

let fromString s =
  try Some (int_of_string s)
  with _ -> None

let toString = string_of_int

type output = char -> unit

(* abstract printer *)
let toBinaryGen (out:output) n =
  let most_significant_bit = (-1) lxor ((-1) lsr 1) in
  let n = if n<0 then (out '-'; -n) else n in
  out '0'; out 'b';
  let rec loop started bit n =
    if bit = 0 then (
      if not started then out '0'
    ) else (
      let b = n land bit in
      if b = 0 then (
        if started then out '0';
        loop started (bit lsr 1) n
      ) else (
        out '1';
        loop true (bit lsr 1) n
      )
    )
  in
  loop false most_significant_bit n

let toBinaryString n =
  let buf = Buffer.create 16 in
  toBinaryGen (Buffer.add_char buf) n;
  Buffer.contents buf

(** These are more specialised than the stdlib general min/max function *)
let min x y = if x - y <= 0 then x else y
let max x y = if x - y >= 0 then x else y

(** For BS backend these values are -2^31 to 2^31-1, for native it depends on the system. *)
let minValue = min_int
let maxValue = max_int
