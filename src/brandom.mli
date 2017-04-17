
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Random Generators} *)


(*-- Start stdlib random, from https://github.com/ocaml/ocaml/blob/4.02.3/stdlib/random.mli --*)

(** {6 Basic functions} *)

val init : int -> unit
(** Initialize the generator, using the argument as a seed.
     The same seed will always yield the same sequence of numbers. *)

val full_init : int array -> unit
(** Same as {!Random.init} but takes more data as seed. *)

val self_init : unit -> unit
(** Initialize the generator with a random seed chosen
    in a system-dependent way.  If [/dev/urandom] is available on
    the host machine, it is used to provide a highly random initial
    seed.  Otherwise, a less random seed is computed from system
    parameters (current time, process IDs). *)

val bits : unit -> int
(** Return 30 random bits in a nonnegative integer.
    @before 3.12.0 used a different algorithm (affects all the following
                   functions)
*)

val int32 : Int32.t -> Int32.t;;
(** [Random.int32 bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. *)

val nativeint : Nativeint.t -> Nativeint.t;;
(** [Random.nativeint bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. *)

val int64 : Int64.t -> Int64.t;;
(** [Random.int64 bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. *)

val bool : unit -> bool
(** [Random.bool ()] returns [true] or [false] with probability 0.5 each. *)


(** {6 Advanced functions} *)

(** The functions from module [State] manipulate the current state
    of the random generator explicitly.
    This allows using one or several deterministic PRNGs,
    even in a multi-threaded program, without interference from
    other parts of the program.
*)

module State : sig
  type t
  (** The type of PRNG states. *)

  val make : int array -> t
  (** Create a new state and initialize it with the given seed. *)

  val make_self_init : unit -> t
  (** Create a new state and initialize it with a system-dependent
      low-entropy seed. *)

  val copy : t -> t
  (** Return a copy of the given state. *)

  val bits : t -> int
  val int : t -> int -> int
  val int32 : t -> Int32.t -> Int32.t
  val nativeint : t -> Nativeint.t -> Nativeint.t
  val int64 : t -> Int64.t -> Int64.t
  val float : t -> float -> float
  val bool : t -> bool
  (** These functions are the same as the basic functions, except that they
      use (and update) the given PRNG state instead of the default one.
  *)
end;;


val get_state : unit -> State.t
(** Return the current state of the generator used by the basic functions. *)

val set_state : State.t -> unit
(** Set the state of the generator used by the basic functions. *)

(*-- End stdlib random --*)


type state = Random.State.t

type 'a t = state -> 'a
(** Random generator for values of type ['a] *)

type 'a random_gen = 'a t

val return : 'a -> 'a t
(** [return x] is the generator that always returns [x].
    Example:  [let random_int = return 4 (* fair dice roll *)] *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t

val (>|=) : 'a t -> ('a -> 'b) -> 'b t

val delay : (unit -> 'a t) -> 'a t
(** Delay evaluation. Useful for side-effectful generators that
    need some code to run for every call.
    Example:
    {[
      let gensym = let r = ref 0 in fun () -> incr r; !r ;;

      delay (fun () ->
          let name = gensym() in
          small_int >>= fun i -> return (name,i)
        )
    ]}
    @since 0.4 *)

val choose : 'a t list -> 'a option t
(** Choose a generator within the list. *)

val choose_exn : 'a t list -> 'a t
(** Same as {!choose} but without option.
    @raise Invalid_argument if the list is empty *)

val choose_array : 'a t array -> 'a option t

val choose_return : 'a list -> 'a t
(** Choose among the list
    @raise Invalid_argument if the list is empty *)

val replicate : int -> 'a t -> 'a list t
(** [replicate n g] makes a list of [n] elements which are all generated
    randomly using [g] *)

val sample_without_replacement:
  ?compare:('a -> 'a -> int) -> int -> 'a t -> 'a list t
(** [sample_without_replacement n g] makes a list of [n] elements which are all
    generated randomly using [g] with the added constraint that none of the generated
    random values are equal
    @raise Invalid_argument if [n <= 0]
    @since 0.15 *)

val list_seq : 'a t list -> 'a list t
(** Build random lists from lists of random generators
    @since 0.4 *)

exception Pick_from_empty
(** @since 0.16 *)

val pick_list : 'a list -> 'a t
(** Pick an element at random from the list
    @raise Pick_from_empty if the list is empty
    @since 0.16 *)

val pick_array : 'a array -> 'a t
(** Pick an element at random from the array
    @raise Pick_from_empty if the array is empty
    @since 0.16 *)

val small_int : int t

val int : int -> int t

val int_range : int -> int -> int t
(** Inclusive range *)

val small_float : float t
(** A reasonably small float.
    @since 0.6.1 *)

val float : float -> float t
(** Random float within the given range
    @since 0.6.1 *)

val float_range : float -> float -> float t
(** Inclusive range. [float_range a b] assumes [a < b].
    @since 0.6.1 *)

val split : int -> (int * int) option t
(** Split a positive value [n] into [n1,n2] where [n = n1 + n2].
    @return [None] if the value is too small *)

val split_list : int -> len:int -> int list option t
(** Split a value [n] into a list of values whose sum is [n]
    and whose length is [length]. The list is never empty and does not
    contain [0].
    @raise Invalid_argument if [len <= 1]
    @return [None] if the value is too small *)

val retry : ?max:int -> 'a option t -> 'a option t
(** [retry g] calls [g] until it returns some value, or until the maximum
    number of retries was reached. If [g] fails,
    then it counts for one iteration, and the generator retries.
    @param max: maximum number of retries. Default [10] *)

val try_successively : 'a option t list -> 'a option t
(** [try_successively l] tries each generator of [l], one after the other.
    If some generator succeeds its result is returned, else the
    next generator is tried *)

val (<?>) : 'a option t -> 'a option t -> 'a option t
(** [a <?> b] is a choice operator. It first tries [a], and returns its
    result if successful. If [a] fails, then [b] is returned. *)

val fix :
  ?sub1:('a t -> 'a t) list ->
  ?sub2:('a t -> 'a t -> 'a t) list ->
  ?subn:(int t * ('a list t -> 'a t)) list ->
  base:'a t -> int t -> 'a t
(** Recursion combinators, for building recursive values.
    The integer generator is used to provide fuel. The [sub_] generators
    should use their arguments only once!
    @param sub1 cases that recurse on one value
    @param sub2 cases that use the recursive gen twice
    @param subn cases that use a list of recursive cases *)

(** {6 Applicative} *)

val pure : 'a -> 'a t

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

(** {6 Run a generator} *)

val run : ?st:state -> 'a t -> 'a
(** Using a random state (possibly the one in argument) run a generator *)

(**/**)

val uniformity_test : ?size_hint:int -> int -> 'a t -> bool t
(** [uniformity_test k rng] tests the uniformity of the random generator [rng] using
    [k] samples.
    @since 0.15
*)
