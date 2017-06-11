(** {2 References on Lists} *)

type 'a t = 'a list ref

val push : 'a t -> 'a -> unit

val pop : 'a t -> 'a option

val popOrRaise : 'a t -> 'a
(** Unsafe version of {!pop}.
    @raise Failure if the list is empty *)

val create : unit -> 'a t
(** Create a new list reference *)

val clear : _ t -> unit
(** Remove all elements *)

val lift : ('a list -> 'b) -> 'a t -> 'b
(** Apply a list function to the content *)

val pushList : 'a t -> 'a list -> unit
(** Add elements of the list at the beginning of the list ref. Elements
    at the end of the list will be at the beginning of the list ref *)