module type ALPHABET_TYPE = sig
  type t

  val compare : t -> t -> int
end

module type DFA_TYPE = sig
  type t
  type q
  type qset
  type s
  type sset

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep

  val fmt_err : err -> string

  val mk_dfa
    :  states:qset
    -> init:q
    -> alpha:sset
    -> final:qset
    -> step:(q -> s -> q)
    -> (t, err) Result.t

  val states : t -> qset
  val init : t -> q
  val alpha : t -> sset
  val final : t -> qset
  val step : t -> q -> s -> (q, err) Result.t
  val step' : t -> q -> s list -> (q, err) Result.t
  val step_tbl : t -> (q * s * q) list
  val accepts : t -> s list -> (bool, err) Result.t
  val cardinal : t -> int
end

module Make (A : ALPHABET_TYPE) :
  DFA_TYPE
  with type q = Int.t
   and type qset = Set.Make(Int).t
   and type s = Set.Make(A).elt
   and type sset = Set.Make(A).t
