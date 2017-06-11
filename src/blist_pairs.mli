
type ('a, 'b) t = ('a * 'b) list

val split : ('a, 'b) t -> 'a list * 'b list
(** Transform a list of pairs into a pair of lists:
    [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
    Not tail-recursive.
*)

val combine : 'a list -> 'b list -> ('a, 'b) t
(** Transform a pair of lists into a list of pairs:
    [combine [a1; ...; an] [b1; ...; bn]] is
    [[(a1,b1); ...; (an,bn)]].
    Raise [Invalid_argument] if the two lists
    have different lengths.  Not tail-recursive. *)