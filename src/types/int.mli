
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Basic Int functions} *)

type t = int

include Comparable.S with type t := t
include Hashable.S with type t := t
include Stringifiable.S with type t:= t
include Bounded.S with type t := t 

val sign : t -> int
(** [sign i] is one of [-1, 0, 1] *)

val neg : t -> t
(** [neg i = - i] *)

val pow : t -> t -> t
(** [pow a b = a^b] for positive integers [a] and [b].
    Raises [Invalid_argument] if [a = b = 0] or [b] < 0. *)

val fromString : string -> t option

val toBinaryString : t -> string

val min : t -> t -> t

val max : t -> t -> t
