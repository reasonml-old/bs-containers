
module type VectorLike = sig 
    type 'a t 

    val mem: 'a Equality.t -> 'a t -> 'a -> bool 
    val length: 'a t -> int
    val isEmpty: 'a t -> bool
    val iter: f:('a -> unit) -> 'a t -> unit
    val fold: f:('acc -> 'a -> 'acc) -> acc:'acc -> 'a t -> 'acc
    val exists: f:('a -> bool) -> 'a t -> bool
    val forAll: f:('a -> bool) -> 'a t -> bool
    val count: f:('a -> bool) -> 'a t -> int
    val find: f:('a -> 'b option) -> 'a t -> 'b option
    val toIterator: 'a t -> 'a Iterator.t 
    val fromIterator: 'a Iterator.t -> 'a t
end