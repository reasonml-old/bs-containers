
(** Basic equality type *)

type 'a t = 'a -> 'a -> bool

let make (compare : 'a -> 'a -> int) : 'a t = 
  fun this that ->
    (let cmp = compare this that in cmp = 0: bool)