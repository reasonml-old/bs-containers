
type +'a node =
| Nil
| Cons of 'a * 'a t
and 'a t = unit -> 'a node

val empty: 'a t

val return: 'a -> 'a t

val map: ('a -> 'b) -> 'a t -> 'b t

val filter: ('a -> bool) -> 'a t -> 'a t

val filterMap: ('a -> 'b option) -> 'a t -> 'b t

val flatMap: ('a -> 'b t) -> 'a t -> 'b t

val foldLeft: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val iter: ('a -> unit) -> 'a t -> unit