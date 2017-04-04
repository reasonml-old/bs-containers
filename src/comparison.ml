type comparison =
  | Greater
  | Less
  | Equal

let compare a b = match Pervasives.compare a b with
  | 0 -> Equal
  | -1 -> Less
  | 1 -> Greater
  | _ -> assert false

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