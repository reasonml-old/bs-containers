type ('k, 'v) t = ('k * 'v) list

let removeByReference = List.remove_assq
let includesByReference = List.mem_assq
let findByReference = List.assq
let find = List.assoc

let rec search_exn eq l x = match l with
  | [] -> raise Not_found
  | (y,z)::l' ->
    if eq x y then z else search_exn eq l' x

let getOrRaise ?(eq=(=)) x l = search_exn eq l x

let get ?(eq = (=)) x l =
  try Some (search_exn eq l x)
  with Not_found -> None

(*$T
  Assoc.get 1 [1, "1"; 2, "2"] = Some "1"
  Assoc.get 2 [1, "1"; 2, "2"] = Some "2"
  Assoc.get 3 [1, "1"; 2, "2"] = None
  Assoc.get 42 [] = None
*)

(* search for a binding for [x] in [l], and calls [f x (Some v) rest]
    or [f x None rest] depending on whether it finds the binding.
    [rest] is the list of the other bindings *)
let rec search_set eq acc l x ~f = match l with
  | [] -> f x None acc
  | (x',y')::l' ->
    if eq x x'
    then f x (Some y') (List.rev_append acc l')
    else search_set eq ((x',y')::acc) l' x ~f

let set ?(eq=(=)) x y l =
  search_set eq [] l x
    ~f:(fun x _ l -> (x,y)::l)

(*$T
  Assoc.set 2 "two" [1,"1"; 2, "2"] |> List.sort Pervasives.compare \
    = [1, "1"; 2, "two"]
  Assoc.set 3 "3" [1,"1"; 2, "2"] |> List.sort Pervasives.compare \
    = [1, "1"; 2, "2"; 3, "3"]
*)

let includes ?(eq = (=)) x l =
  try ignore (search_exn eq l x); true
  with Not_found -> false

(*$T
  Assoc.mem 1 [1,"1"; 2,"2"; 3, "3"]
  not (Assoc.mem 4 [1,"1"; 2,"2"; 3, "3"])
*)

let update ?(eq = (=)) ~f x l =
  search_set eq [] l x
    ~f:(fun x opt_y rest ->
        match f opt_y with
        | None -> rest (* drop *)
        | Some y' -> (x,y') :: rest)
(*$=
  [1,"1"; 2,"22"] \
    (Assoc.update 2 [1,"1"; 2,"2"] \
      ~f:(function Some "2" -> Some "22" | _ -> assert false) |> lsort)
  [1,"1"; 3,"3"] \
    (Assoc.update 2 [1,"1"; 2,"2"; 3,"3"] \
      ~f:(function Some "2" -> None | _ -> assert false) |> lsort)
  [1,"1"; 2,"2"; 3,"3"] \
    (Assoc.update 3 [1,"1"; 2,"2"] \
      ~f:(function None -> Some "3" | _ -> assert false) |> lsort)
*)

let remove ?(eq=(=)) x l =
  search_set eq [] l x
    ~f:(fun _ opt_y rest -> match opt_y with
        | None -> l  (* keep as is *)
        | Some _ -> rest)

(*$=
  [1,"1"] \
    (Assoc.remove 2 [1,"1"; 2,"2"] |> lsort)
  [1,"1"; 3,"3"] \
    (Assoc.remove 2 [1,"1"; 2,"2"; 3,"3"] |> lsort)
  [1,"1"; 2,"2"] \
    (Assoc.remove 3 [1,"1"; 2,"2"] |> lsort)
*)