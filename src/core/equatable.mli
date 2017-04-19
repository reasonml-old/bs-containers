(** Represents modules have equality *)

module type S  = sig 
  type t 
  val equals : t Equality.t 
end

(** C stands for containers *)
module type C = sig 
  type 'a t
  val equals : 'a Equality.t -> 'a t Equality.t
end