type comparison =
  | Greater
  | Less
  | Equal

let compare a b = match Pervasives.compare a b with
  | 0 -> Equal
  | -1 -> Less
  | 1 -> Greater
  | _ -> assert false