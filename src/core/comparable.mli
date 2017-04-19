(** Represents module that can be compared *)

module type S = sig
  type t

  include Equatable.S with type t := t

  val compare: t Comparator.t
end

module type C = sig
  type 'a t 
  include Equatable.C with type 'a t := 'a t
  val compare: 'a  Comparator.t -> 'a t Comparator.t
end
