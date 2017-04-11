(** Represents modules have equality *)

module type S  = sig 
  type t 
  val equals : t Equality.t 
end