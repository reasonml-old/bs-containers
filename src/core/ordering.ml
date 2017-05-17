(** A type represent a total order relationship *)

type t =
    | Greater
    | Less
    | Equal

let fromInt i = 
    if i > 0 
    then Greater
    else (
        if i = 0 then Equal
        else Less
    ) 

let toInt = function
    | Greater -> 1
    | Less -> -1 
    | Equal -> 0

let greater = Greater

let less = Less

let equal = Equal