(** Represents module that can be compared *)

module type S = sig
  type t

  include Equatable.S with type t := t

  val compare: t Comparator.t
end