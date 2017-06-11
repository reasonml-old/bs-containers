
type +'a node =
  | Nil
  | Cons of 'a * 'a t
and 'a t = unit -> 'a node

let empty () = Nil

let return x () = Cons(x, empty)

let rec map f i () = 
  match i () with 
  | Nil -> Nil
  | Cons (x, r) -> Cons (f x, map f r)

let rec filter p i () = 
  match i () with 
  | Nil -> Nil
  | Cons (x, r) -> if p x then Cons (x, filter p r) else filter p r ()

let rec filterMap p i () = 
  match i () with
  | Nil -> Nil
  | Cons (x, r) -> 
    begin match p x with 
    | Some c -> Cons (c, filterMap p r)
    | None -> filterMap p r ()
    end

(** Append iterator y after iterator x *)
let rec append x y = 
  match x () with
  | Nil -> y ()
  | Cons (xc, r) -> Cons (xc, fun () -> append r y)

let rec flatMap trans x () = 
  match x () with
  | Nil -> Nil
  | Cons (this, next) -> append (trans this) (flatMap trans next)

let rec reduce trans acc iter = 
  match iter () with
  | Nil -> acc
  | Cons (x, next) -> reduce trans (trans acc x) next

let rec forEach f i = 
  match i () with
  | Nil -> ()
  | Cons (x, next) -> f x; forEach f next
