(* This file is free software, part of containers. See file "license" for more details. *)

(** Module for stdlib bool type *)

type t = bool

let negate (x:t) : t = not x;;

let equals = fun a b -> a == b

let compare (a:bool) b = match a,b with
  | true, false -> Ordering.Greater
  | false, true -> Ordering.Less
  | _, _ -> Ordering.Equal

let toString = function 
| true -> "true"
| false -> "false"
