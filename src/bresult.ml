
(* This file is free software, part of containers. See file "license" for more details. *)

(*

- [x] Use `camelCase` instead of `snake_case`
- [x] Get rid of scary monadic and math-y terminology
- [x] Follow the conventions used in the `Js.*` modules
- [ ] Type functions correctly, e.g. `compare` should return a proper variant, not `0`, `1` or `-1`
- [x] Remove operators and aliases, e.g. `pure` as an alias for `return`
- [ ] Remove the use of exceptions for internal logic (maybe? seems very sketchy to me but perhaps there's a really really good reason, see [this example](https://github.com/BuckleTypes/bs-containers/blob/master/src/bopt.ml#L163))
- [ ] Replace `Format.printf` calls, they pull in a lot stuff for little benefit
- [ ] Document everything properly, with examples
- [ ] Add tests for everything

*)

(** {1 Error Monad} *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> Comparison.comparison

(** {2 Basics} *)

type (+'good, +'bad) t = ('good, 'bad) Result.result =
  | Ok of 'good
  | Error of 'bad

let make x = Ok x

let fail s = Error s

let fromException e =
  let msg = Printexc.to_string e in
  Error msg

let fromExceptionTrace e =
  let res = Printf.sprintf "%s\n%s"
      (Printexc.to_string e) (Printexc.get_backtrace ())
  in
  Error res

let map f e = match e with
  | Ok x -> Ok (f x)
  | Error s -> Error s

let mapError f e = match e with
  | Ok _ as res -> res
  | Error y -> Error (f y)

let map2 f g e = match e with
  | Ok x -> Ok (f x)
  | Error s -> Error (g s)

let forEach f e = match e with
  | Ok x -> f x
  | Error _ -> ()

exception GetError

let getOrRaise = function
  | Ok x -> x
  | Error _ -> raise GetError

let getOr e ~default = match e with
  | Ok x -> x
  | Error _ -> default

let mapOr f e ~default = match e with
  | Ok x -> f x
  | Error _ -> default

let catch e ~ok ~err = match e with
  | Ok x -> ok x
  | Error y -> err y

let flatMap f e = match e with
  | Ok x -> f x
  | Error s -> Error s

let equal ?(err=Pervasives.(=)) eq a b = match a, b with
  | Ok x, Ok y -> eq x y
  | Error s, Error s' -> err s s'
  | _ -> false

let compare ?(err=Comparison.compare) cmp a b = match a, b with
  | Ok x, Ok y -> cmp x y
  | Ok _, _  -> Comparison.Greater
  | _, Ok _ -> Comparison.Less
  | Error s, Error s' -> err s s'

let reduce ~ok ~error x = match x with
  | Ok x -> ok x
  | Error s -> error s

let isOk = function
  | Ok _ -> true
  | Error _ -> false

let isError = function
  | Ok _ -> false
  | Error _ -> true

(** {2 Wrappers} *)

let guard f =
  try Ok (f ())
  with e -> Error e

let guardToString f =
  try Ok (f())
  with e -> fromException e

let guardToStringTrace f =
  try Ok (f())
  with e -> fromExceptionTrace e

let wrap1 f x =
  try make (f x)
  with e -> Error e

let wrap2 f x y =
  try make (f x y)
  with e -> Error e

let wrap3 f x y z =
  try make (f x y z)
  with e -> Error e

(** {2 Applicative} *)

let apply f x = match f with
  | Error s -> fail s
  | Ok f -> map f x

let join t = match t with
  | Ok (Ok o) -> Ok o
  | Ok (Error e) -> Error e
  | (Error _) as e -> e

let both x y = match x,y with
  | Ok o, Ok o' -> Ok (o, o')
  | Ok _, Error e -> Error e
  | Error e, _  -> Error e

(** {2 Collections} *)

let mapList f l =
  let rec map acc l = match l with
    | [] -> Ok (List.rev acc)
    | x::l' ->
      match f x with
      | Error s -> Error s
      | Ok y -> map (y::acc) l'
  in map [] l

exception LocalExit

let foldSeq f acc seq =
  let err = ref None in
  try
    let acc = ref acc in
    seq
      (fun x -> match f !acc x with
         | Error s -> err := Some s; raise LocalExit
         | Ok y -> acc := y);
    Ok !acc
  with LocalExit ->
  match !err with None -> assert false | Some s -> Error s

let foldList f acc l = foldSeq f acc (fun k -> List.iter k l)

(** {2 Misc} *)

let choose l =
  let rec find_ = function
    | [] -> raise Not_found
    | ((Ok _) as res) :: _ -> res
    | (Error _) :: l' -> find_ l'
  in
  try find_ l
  with Not_found ->
    let l' = List.map (function Error s -> s | Ok _ -> assert false) l in
    Error l'

let retry n f =
  let rec retry n acc = match n with
    | 0 -> fail (List.rev acc)
    | _ ->
      match f () with
      | Ok _ as res -> res
      | Error e -> retry (n-1) (e::acc)
  in retry n []

(** {2 Conversions} *)

let toOption = function
  | Ok x -> Some x
  | Error _ -> None

let fromOption = function
  | None -> Error "of_opt"
  | Some x -> Ok x

let toSeq e k = match e with
  | Ok x -> k x
  | Error _ -> ()