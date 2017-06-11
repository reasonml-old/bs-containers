
module type S  = sig 
  type t 
  val equals : t Equality.t 
end