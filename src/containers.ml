
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Drop-In replacement to Stdlib} *)

module Array = Barray
module ArrayLabels = ArrayLabels
module Array_slice = Barray_slice
module Bool = Bool
module Char = Bchar
module Float = Bfloat
module Format = Bformat
module Fun = Bfun
module Hash = Bhash
module Hashtbl = Bhashtbl
module Heap = Bheap
module Int = Int
module Int64 = Bint64
module List = struct
  include Blist
  
  module Association = Blist_association
  module Pairs = Blist_pairs
  module Ref = Blist_ref
  module Ex = Blist_ex
end
module ListLabels = Blistlabels
module Map = Bmap
module Option = Bopt
module Ord = Bord
module Pair = Bpair
module Parse = Bparse
module Random = Brandom
module Ref = Bref
module Result = Bresult
module Set = Bset
module Sequence = Sequence
module String = Bstring
module Vector = Bvector