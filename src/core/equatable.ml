
module type S  = sig 
  type t 
  val equals : t Equality.t 
end

module type C = sig 
  type 'a t
  val equals : 'a Equality.t -> 'a t Equality.t
end