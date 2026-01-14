module type ALPHABET_TYPE = sig
  type t

  val compare : t -> t -> int
end

module type NFA_TYPE = sig
  type t
  type q
  type qset
  type s
  type sset

  type ts =
    | Sym of s
    | Eps

  type err =
    | InvalidChar
    | EmptyStates
    | EmptyAlpha
    | InvalidInit
    | InvalidFinal
    | InvalidStep
    | AlphaMismatch

  val mk_nfa
    :  states:qset
    -> init:q
    -> alpha:sset
    -> final:qset
    -> step:(q -> ts -> qset)
    -> (t, err) Result.t

  val states : t -> qset
  val init : t -> q
  val alpha : t -> sset
  val final : t -> qset
  val step : t -> q -> s -> (qset, err) Result.t
  val step' : t -> q -> s list -> (qset, err) Result.t
  val step_tbl : t -> (q * ts * qset) list
  val accepts : t -> s list -> (bool, err) Result.t
  val closure : t -> qset -> qset
  val cardinal : t -> int
  val union : t -> t -> (t, err) Result.t
  val concat : t -> t -> (t, err) Result.t
  val kstar : t -> t
  val complement : t -> t
end

module Make (A : ALPHABET_TYPE) :
  NFA_TYPE
  with type q = Int.t
   and type qset = Set.Make(Int).t
   and type s = Set.Make(A).elt
   and type sset = Set.Make(A).t
